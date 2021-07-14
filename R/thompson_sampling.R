thompson_sampling <- function(k, alpha = 1, beta = 1) {
  force(alpha)
  force(beta)
  force(k)
  Mu <- matrix(Inf, nrow = 1, ncol = k)
  Nu <- matrix(0, nrow = 1, ncol = k)
  t <- 1

  whatnext <- function() {
    if (t <= k) {
      list(which=t, proba=Inf)
    }
    else {
      indices <- mapply(ts, Mu, Nu, MoreArgs = list(alpha, beta))
      list(which=which.max(indices), proba=max(indices))
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
    t <<- t+1
  }

  list(whatnext=whatnext, nowwhat=nowwhat)
}


ts <- function(mu, nu, alpha, beta) {
  rbeta(1, alpha + mu*nu, beta + nu - mu*nu)
}
