thompson_sampling <- function(k, alpha = 1, beta = 1) {
  force(alpha)
  force(beta)
  force(k)
  Mu <- matrix(Inf, nrow = 1, ncol = k)
  Nu <- matrix(0, nrow = 1, ncol = k)
  t <- 1

  choose <- structure(function() {
    if (t <= k) {
      data.frame(which=t, tsample=Inf, stringsAsFactors = FALSE)
    }
    else {
      indices <- mapply(ts, Mu, Nu, alpha, beta)
      data.frame(which=which.max(indices), tsample=max(indices), stringsAsFactors = FALSE)
    }
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


ts <- function(mu, nu, alpha, beta) {
  out <- rbeta(1, alpha + mu*nu, beta + nu - mu*nu)
  out
}
