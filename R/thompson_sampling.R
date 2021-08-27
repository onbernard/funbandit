#' @include make_policy.R
NULL

# =============================

init_thompson_sampling <-
  function(k, PolArgs = list(alpha = 1, beta = 1)) {
    alpha <- as.integer(PolArgs$alpha)
    beta <- as.integer(PolArgs$beta)
    k <- as.integer(k)

    Mu <- rep(Inf, k)
    Nu <- rep.int(0, k)
    t <- 1
    list(
      alpha = alpha,
      beta = beta,
      k = k,
      Mu = Mu,
      Nu = Nu,
      t = t
    )
  }

choose_thompson_sampling <- function() {
  if (t <= k) {
    list(which = t, tsample = Inf)
  }
  else {
    indices <- mapply(ts, Mu, Nu, alpha, beta)
    list(which = which.max(indices), tsample = max(indices))
  }
}

receive_thompson_sampling <- function(arm, reward) {
  if (Nu[arm] == 0) {
    Mu[arm] <<- reward
  }
  else {
    Mu[arm] <<- ((Mu[arm] * Nu[arm] + reward) / (Nu[arm] + 1))
  }
  Nu[arm] <<- Nu[arm] + 1
  t <<- t + 1
}

#' Thompson sample value of an arm
#'
#' @param mu observed mean
#' @param nu number of trials
#' @param alpha first beta distribution parameter
#' @param beta second beta distribution parameter
#'
#' @return double value
#' @export
#'
#' @examples
ts <- function(mu, nu, alpha, beta) {
  out <- rbeta(1, alpha + mu * nu, beta + nu - mu * nu)
  out
}

thompson_sampling_policy <-
  make_policy(
    init_thompson_sampling,
    choose_thompson_sampling,
    receive_thompson_sampling,
    "thompson_sampling"
  )
