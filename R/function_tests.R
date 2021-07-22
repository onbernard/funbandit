gen_data <- function(n) {
  outp <- matrix(NA, ncol=5, nrow=n)
  for(a in 1:5) {
    outp[,a] <- rbinom(n, 1, runif(1))
  }
  outp
}

test_appl <- function(which_apply, which_pol, upto) {
  force(which_apply)
  force(which_pol)
  force(upto)
  data <- gen_data(upto)
  function(n) {
    which_apply(which_pol, data[seq_len(n),])
  }
}

apply_policy_rbind <- function(policy, rewards, PolicyArgs=NULL) {
  rewards <- as.matrix(rewards)
  pol <- c()
  if (is.null(PolicyArgs)) {
    pol <- policy(k=ncol(rewards))
  }
  else {
    pol <- policy(k=ncol(rewards), unlist(PolicyArgs))
  }

  h <- nrow(rewards)
  k <- ncol(rewards)
  mkRow <- fancy_results(pol, rewards)

  d <- mkRow(1)
  nCol <- ncol(d)
  for(i in 2:h) {
    d <- rbind(d, mkRow(i))
  }
  d
}

apply_policy_nohack <- function(policy, rewards, PolicyArgs=NULL) {
  rewards <- as.matrix(rewards)
  pol <- c()
  if (is.null(PolicyArgs)) {
    pol <- policy(k=ncol(rewards))
  }
  else {
    pol <- policy(k=ncol(rewards), unlist(PolicyArgs))
  }

  h <- nrow(rewards)
  k <- ncol(rewards)
  mkRow <- fancy_results(pol, rewards)

  d <- mkRow(1)
  d <- d[rep.int(1,h),]
  nCol <- ncol(d)
  if (h>1) {
    d <- d[rep.int(1,h),]
    for (i in seq.int(2,h,1)) {
      ri <- mkRow(i)
      for (j in seq_len(nCol)) {
        d[[j]][i] <- ri[[j]]
      }
    }
  }
  d
}

test_linucb <- function() {
  size.tot = 1000
  set.seed(4649)                          # this makes the example exactly reproducible
  x1 = runif(size.tot, min=0, max=10)          # you have 4, largely uncorrelated predictors
  x2 = runif(size.tot, min=0, max=10)
  x3 = runif(size.tot, min=0, max=10)
  x4 = runif(size.tot, min=0, max=10)
  dt = cbind(x1,x2,x3,x4)
  #arm reward
  arm_1 <-  as.vector(c(-1,9,-8,4))
  K1 = crossprod(t(dt),arm_1)
  arm_2 <-  as.vector(c(-1,2,1,0))
  K2 = crossprod(t(dt),arm_2)
  arm_3 <-  as.vector(c(-1,-5,1,10))
  K3 = crossprod(t(dt),arm_3)
  visitor_reward <-  cbind(K1,K2,K3)

  pol <- linear_upper_confidence_bound(3, 4)

  choices <- rep(0,1000)
  proba <- rep(0, 1000)
  for(i in 1:1000) {
    whatnext <- pol$choose(dt[i,])
    choices[i] <- whatnext$which
    proba[i] <- whatnext$maxucb
    pol$receive(choices[i], visitor_reward[i,choices[i]], dt[i,])
  }
  choices
}

# TODO : test apply as an aggregate of reduce
