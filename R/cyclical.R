

init_cyclical <- function(k) {
  k <- as.integer(k)
  Mu <- rep(Inf, k)
  Nu <- rep.int(0, k)
  t <- 1
  list(k = k,
       Mu = Mu,
       Nu = Nu,
       t = t)
}

choose_cyclical <- function() {
  list(which = t, expect = Mu[t])
}

receive_cyclical <- function(arm, reward) {
  if (Nu[arm] == 0) {
    Mu[arm] <<- reward
  }
  else {
    Mu[arm] <<- ((Mu[arm] * Nu[arm] + reward) / (Nu[arm] + 1))
  }
  Nu[arm] <<- Nu[arm] + 1
  t <<- t %% k + 1
}

