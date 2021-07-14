epsilon_greedy <- function(k, epsilon = 0.25) {
  force(epsilon)
  force(k)
  Mu <- matrix(Inf, nrow = 1, ncol = k)
  Nu <- matrix(0, nrow = 1, ncol = k)
  t <- 1

  whatnext <- function() {
    if (t <= k) {
      list(which = t, proba = Inf, reason="explore")
    }
    else {
      whatdo <- exploit_or_not(epsilon)
      which <- switch(
        whatdo,
        "exploit" = which.max(Mu),
        "explore" = sample(1:k, size = 1, replace = TRUE)
      )
      list(which = which, proba = Mu[which], reason=whatdo)
    }
  }

  nowwhat <- function(arm, reward) {
    if (Nu[arm] == 0) {
      Mu[arm] <<- reward
    }
    else {
      Mu[arm] <<- ((Mu[arm] * Nu[arm] + reward) / (Nu[arm] + 1))
    }
    Nu[arm] <<- Nu[arm] + 1
    t <<- t + 1
  }

  list(whatnext = whatnext, nowwhat = nowwhat)
}

exploit_or_not <- function(epsilon) {
  sample(
    c("exploit", "explore"),
    size = 1,
    replace = TRUE,
    prob = c(1 - epsilon, epsilon)
  )
}
