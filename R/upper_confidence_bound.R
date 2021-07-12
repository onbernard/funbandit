upper_confidence_bound <- function(k, alpha=1) {
  force(alpha)
  force(k)
  Mu <- matrix(Inf, nrow = 1, ncol = k)
  Nu <- matrix(0, nrow = 1, ncol = k)
  t <- 0

  whatnext <- function() {
    index <- mapply(ucb, Mu, Nu, MoreArgs = list(alpha, t))
    list(which=which.max(index), proba=max(index))
  }
  nowwhat <- function(arm, reward) {
    if (Nu[arm] == 0) {
      Mu[arm] <<- reward
    }
    else {
      Mu[arm] <<- ((Mu[arm] * Nu[arm] + reward) / (Nu[arm] + 1))
    }
    Nu[arm] <<- Nu[arm] + 1
    t <<- t+1
  }

  list(whatnext=whatnext, nowwhat=nowwhat)
}


ucb <- function(mu, nu, alpha, t) {
  if (nu == 0) {
    Inf
  }
  else {
    mu + alpha * sqrt((2 * log(t)) / nu)
  }
}


round <- function(policy) {
  function(rewards) {
    choice <- policy$whatnext()$which
    policy$nowwhat(choice, rewards[choice])
  }
}
