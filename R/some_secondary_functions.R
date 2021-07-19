pretty_results <- function(policy, rewards) {
  force(policy)
  force(rewards)
  k <- nrow(rewards)

  Mu <- matrix(0, nrow = 1, ncol = k)
  Nu <- matrix(0, nrow = 1, ncol = k)
  cum_regret <- 0

  function(t) {
    whatnext <- policy$choose()
    choice <- whatnext$which
    policy$receive(choice, rewards[t,choice])
    regret <- max(rewards[t,]) - rewards[t,choice]
    cum_regret <<- cum_regret + regret
    outp <- bind_cols(whatnext, expectation = Mu[choice], reward = rewards[t,choice], cum_regret=cum_regret)
    if (Nu[choice] == 0) {
      Mu[choice] <<- rewards[t,choice]
    }
    else {
      Mu[choice] <<-
        ((Mu[choice] * Nu[choice] + rewards[t,choice]) / (Nu[choice] + 1))
    }
    Nu[choice] <<- Nu[choice] + 1
    outp
  }
}


apply_policy <- function(policy, rewards, PolicyArgs=NULL, name=character()) {
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
  mkRow <- pretty_results(pol, rewards)

  d <- mkRow(1)
  d <- d[rep.int(1,h),]
  nCol <- ncol(d)
  if (h>1) {
    d <- d[rep.int(1,h),]
    d <- as.list(d)
    for (i in seq.int(2,h,1)) {
      ri <- mkRow(i)
      for (j in seq_len(nCol)) {
        d[[j]][i] <- ri[[j]]
      }
    }
  }
  d <- do.call(bind_cols, d)
  # print(M)
  # output <- tibble(M)
  # colnames(output) <- name
  # output
  #d["which"] <- factor(d[["which"]])
  d
}

