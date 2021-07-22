upper_confidence_bound <- function(k, alpha=1) {
  force(k)
  force(alpha)
  stopifnot(is.double(alpha))
  stopifnot(is.integer(k))
  Mu <- matrix(Inf, nrow = 1, ncol = k)
  Nu <- matrix(0, nrow = 1, ncol = k)
  t <- 1

  choose <- structure(function() {
    indices <- mapply(ucb, Mu, Nu, MoreArgs = list(alpha, t))
    which <- which.max(indices)
    maxucb <- max(indices)
    if (maxucb == Inf) {
      maxucb <- NA
    }
    data.frame(which=which, maxucb=maxucb, stringsAsFactors = FALSE)
  }, class="agent.choose")

  receive <- structure(function(arm, reward) {
    if (Nu[arm] == 0) {
      Mu[arm] <<- reward
    }
    else {
      Mu[arm] <<- ((Mu[arm] * Nu[arm] + reward) / (Nu[arm] + 1))
    }
    Nu[arm] <<- Nu[arm] + 1
    t <<- t+1
  }, class="agent.receive")

  structure(list(choose=choose, receive=receive), class="agent")
}


ucb <- function(mu, nu, alpha, t) {
  if (nu == 0) {
    Inf
  }
  else {
    mu + alpha * sqrt((2 * log(t)) / nu)
  }
}
