#' @include make_policy.R
NULL
# =============================

init_kl_ucb <- function(k, c=0) {
  k <- as.integer(k)
  c <- as.double(c)

  Mu <- rep(Inf, k)
  Nu <- rep.int(0, k)
  t <- 1
  precision <- 1e-6
  list(
    k = k,
    c = c,
    Mu = Mu,
    Nu = Nu,
    t = t,
    precision = precision
  )
}

choose_kl_ucb <- function() {
  if (t <= k) {
    list(which = t)

  }
  else{
    D <- (log(t) + c * (log(log(t)))) / Nu
    upperbounds <- min(1, kl_ucb_gaussian(Mu, D))
    lowerbounds <- Mu

    indices <-
      root_search(
        lowerbound = lowerbounds,
        upperbound = upperbounds,
        center = D,
        f = Vectorize(kl_bernoulli),
        fargs = list(Mu)
      )

    list(which = which.max(indices))
  }
}

receive_kl_ucb <- function(arm, reward) {
  if (Nu[arm] == 0) {
    Mu[arm] <<- reward
  }
  else {
    Mu[arm] <<- ((Mu[arm] * Nu[arm] + reward) / (Nu[arm] + 1))
  }
  Nu[arm] <<- Nu[arm] + 1
  t <<- t + 1
}

root_search <-
  function(lowerbound,
           upperbound,
           center = 0,
           f,
           fargs = list(),
           precision = 1e-6,
           max_iteration = 50) {
    iteration_count <- 0
    while (iteration_count < max_iteration &&
           upperbound - lowerbound > precision) {
      m <- (lowerbound + upperbound) / 2
      lowerbound <-
        ifelse(do.call(f, c(list(m), fargs)) <= center, m, lowerbound)
      upperbound <-
        ifelse(do.call(f, c(list(m), fargs)) > center, m, upperbound)
      iteration_count <- iteration_count + 1
    }
    (lowerbound + upperbound) / 2
  }



kl_ucb_gaussian <- function(x, d, sigma2 = 1) {
  return (x + sqrt(2 * sigma2 * d))

}

kl_gaussian <- function(mu1,
                        mu2,
                        sig1 = 1,
                        sig2 = 1) {
  log(sig2 / sig1) + (sig1 ^ 2 + (mu1 - mu2) ^ 2) / (2 * sig2 ^ 2) - 1 / 2
}

kl_bernoulli <- function(p, q) {
  epsilon <- 1e-16
  p = min(max(p, epsilon), 1 - epsilon)
  q = min(max(q, epsilon), 1 - epsilon)

  return(p * log(p / q) + (1 - p) * log((1 - p) / (1 - q)))

}


