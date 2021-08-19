#
#
# test_appl <- function(which_apply, which_pol, upto) {
#   force(which_apply)
#   force(which_pol)
#   force(upto)
#   data <- gen_data(upto)
#   function(n) {
#     which_apply(which_pol, data[seq_len(n),])
#   }
# }
#
# apply_policy_rbind <- function(policy, rewards, PolicyArgs=NULL) {
#   rewards <- as.matrix(rewards)
#   pol <- c()
#   if (is.null(PolicyArgs)) {
#     pol <- policy(k=ncol(rewards))
#   }
#   else {
#     pol <- policy(k=ncol(rewards), unlist(PolicyArgs))
#   }
#
#   h <- nrow(rewards)
#   k <- ncol(rewards)
#   mkRow <- fancy_results(pol, rewards)
#
#   d <- mkRow(1)
#   nCol <- ncol(d)
#   for(i in 2:h) {
#     d <- rbind(d, mkRow(i))
#   }
#   d
# }
#
# apply_policy_nohack <- function(policy, rewards, PolicyArgs=NULL) {
#   rewards <- as.matrix(rewards)
#   pol <- c()
#   if (is.null(PolicyArgs)) {
#     pol <- policy(k=ncol(rewards))
#   }
#   else {
#     pol <- policy(k=ncol(rewards), unlist(PolicyArgs))
#   }
#
#   h <- nrow(rewards)
#   k <- ncol(rewards)
#   mkRow <- fancy_results(pol, rewards)
#
#   d <- mkRow(1)
#   d <- d[rep.int(1,h),]
#   nCol <- ncol(d)
#   if (h>1) {
#     d <- d[rep.int(1,h),]
#     for (i in seq.int(2,h,1)) {
#       ri <- mkRow(i)
#       for (j in seq_len(nCol)) {
#         d[[j]][i] <- ri[[j]]
#       }
#     }
#   }
#   d
# }
#
# apply_policy <- function(policy, rewards, PolicyArgs=NULL) {
#   rewards <- as.matrix(rewards)
#   pol <- c()
#   name <- deparse1(substitute(policy))
#   if (is.null(PolicyArgs)) {
#     pol <- policy(k=ncol(rewards))
#   }
#   else {
#     pol <- policy(k=ncol(rewards), unlist(PolicyArgs))
#     name <- paste(name, deparse1(substitute(PolicyArgs)))
#   }
#
#   h <- nrow(rewards)
#   k <- ncol(rewards)
#   mkRow <- mkRound(pol, rewards)
#
#   d <- mkRow(1)
#   d <- d[rep.int(1,h),]
#   nCol <- ncol(d)
#   if (h>1) {
#     d <- d[rep.int(1,h),]
#     d <- as.list(d)
#     for (i in seq.int(2,h,1)) {
#       ri <- mkRow(i)
#       for (j in seq_len(nCol)) {
#         d[[j]][i] <- ri[[j]]
#       }
#     }
#   }
#   df <- data.frame(d, stringsAsFactors = FALSE)
#   df$pol <- name
#   df$pol <- factor(df$pol)
#   df
# }
#
# # TODO : test apply as an aggregate of reduce

KLUCB <- function(visitor_reward, K = ncol(visitor_reward), precision=1e-6, c=0){

  choice <- c()
  indice <- c()
  S <- GenerateMatrixS(K)

  tic()

  for (i in 1:K){
    choice[i] <- i
    S <- PlayArm(iter=i, arm=i, S=S, visitor_reward)
  }

  for (j in (K+1):nrow(visitor_reward)){

    t=sum(S[2,])
    if(t<10){print((log(t) + c*(log(log(t)))) / S[2,])}
    indice <- kl_ucb_bernoulli(S[1,], d=(log(t) + c*(log(log(t)))) / S[2,] , precision=precision, S=S,
                               max_iteration = 50, visitor_reward=visitor_reward[j,])
    choice[j] <- which.max(indice)
    S <- PlayArm(iter=j, arm=choice[j], S=S, visitor_reward)

  }

  time <- toc()

  #coef estimate
  th_hat = S[1,]

  #real coef
  th = colMeans(visitor_reward)

  message("th_hat")
  message(th_hat)
  message("th real")
  message(th)


  return(list('S'=S, 'time'=(time$toc - time$tic),'choice'= choice,'theta_hat'=th_hat,'theta'=th))
}

GenerateMatrixS <- function(x) {
  S <- matrix(rep(0,2*x), nrow = 2, ncol = x)
  colnames(S) <- paste('bandit', 1:x)
  rownames(S) <- c("average reward","trials")
  return (S)
}

PlayArm <- function(iter, arm, S, visitor_reward) {
  #mean
  S[1,arm] <- ((S[1,arm] * S[2,arm] + visitor_reward[iter,arm]) / (S[2,arm] + 1))
  #play
  S[2,arm] = S[2,arm] + 1
  return (S)
}

ConditionForKLUCB <- function(kl, S, d, upperbound, precision, max_iteration, visitor_reward){

  # Bisection method

  upperbound = upperbound
  reward = S[1,]
  value = reward
  count_iteration = 0
  kl_vec <- Vectorize(kl)

  while (count_iteration < max_iteration && upperbound - value > precision){
    m = (value + upperbound) / 2

    upperbound <- ifelse(kl_vec(reward, m) > d, m, upperbound)
    value <- ifelse(kl_vec(reward, m) <= d, m, value)

    count_iteration = count_iteration + 1
  }

  return ((value + upperbound) / 2)

}




kl_ucb_bernoulli <- function(x, d, precision, S, max_iteration, visitor_reward){

  upperbound = min(1, kl_ucb_gaussian(x, d))

  return (ConditionForKLUCB(kl=kl_bernoulli, S=S, d=d, precision=precision, upperbound = upperbound,
                            max_iteration = max_iteration, visitor_reward = visitor_reward[j, ]
  ))
}
