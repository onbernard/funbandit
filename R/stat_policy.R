#' @export
stat_policy <- function(policy, polArgs=list()) {
  UseMethod("stat_policy")
}

#' @export
stat_policy.policy <- function(policy, polArgs=list()) {
  force(policy)
  force(polArgs)

  pol_name <- deparse1(substitute(policy))
  arg_name <- paste(mapply(function(n, v){paste(list(n,v), collapse="=")}, names(polArgs), polArgs), collapse=" ")
  pol_name <- paste(c(pol_name, "(", arg_name, ")"), collapse="")


  Mu <- NULL
  Nu <- NULL
  cum_regret <- 0
  t <- 1

  agent <- NULL
  k <- -1

  function(reward_data) {
    rewardmat <- prepare_reward_data(reward_data)
    if (k == -1) {
      k <<- ncol(rewardmat)
    }
    stopifnot(k == ncol(reward_data))
    if (is.null(agent)) {
      agent <<- do.call(policy, c(list(k=k), polArgs))
    }
    Mu <<- matrix(0, nrow = 1, ncol = k)
    Nu <<- matrix(0, nrow = 1, ncol = k)

    h <- nrow(rewardmat)

    whatnext <- agent$choose()
    choice <- whatnext$which
    agent$receive(choice, rewardmat[1,choice])
    regret <- max(rewardmat[1,]) - rewardmat[1,choice]
    cum_regret <<- cum_regret + regret
    myrow <- cbind(whatnext, expectation = Mu[choice], reward = rewardmat[1,choice], cum_regret=cum_regret, t=t)
    if (Nu[choice] == 0) {
      Mu[choice] <<- rewardmat[1,choice]
    }
    else {
      Mu[choice] <<-
        ((Mu[choice] * Nu[choice] + rewardmat[1,choice]) / (Nu[choice] + 1))
    }
    Nu[choice] <<- Nu[choice] + 1
    t <<- t+1

    d <- myrow
    nCol <- ncol(d)
    if (h>1) {
      d <- d[rep.int(1,h),]
      d <- as.list(d) # hack
      for (i in seq.int(2,h,1)) {

        whatnext <- agent$choose()
        choice <- whatnext$which
        agent$receive(choice, rewardmat[i,choice])
        regret <- max(rewardmat[i,]) - rewardmat[i,choice]
        cum_regret <<- cum_regret + regret
        myrow <- cbind(whatnext, expectation = Mu[choice], reward = rewardmat[i,choice], cum_regret=cum_regret, t=t)
        if (Nu[choice] == 0) {
          Mu[choice] <<- rewardmat[i,choice]
        }
        else {
          Mu[choice] <<-
            ((Mu[choice] * Nu[choice] + rewardmat[i,choice]) / (Nu[choice] + 1))
        }
        Nu[choice] <<- Nu[choice] + 1
        t <<- t+1

        for (j in seq_len(nCol)) {
          d[[j]][i] <- myrow[[j]]
        }
      }
    }
    df <- data.frame(d, stringsAsFactors = FALSE)
    df$policy <- pol_name
    df$policy <- factor(df$policy)
    df
  }
}

stat_policy.ctxt_policy <- function(policy, polArgs=list()) {
  function(reward_data, context_data) {

  }
}
