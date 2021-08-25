#' Thompson Sampling Policy
#'
#' @param k number of arms
#' @param alpha first beta distribution parameter
#' @param beta second beta distribution parameter
#'
#' @return An agent object, i.e. a list of two functions : choose and receive
#' @export
#'
#' @examples
thompson_sampling <- structure(function(k, alpha = 1, beta = 1) {
  alpha <- as.integer(alpha)
  beta <- as.integer(beta)
  k <- as.integer(k)

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

  structure(list(choose=choose, receive=receive), k=k, name="ts", class="agent")
}, class="policy")


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
  out <- rbeta(1, alpha + mu*nu, beta + nu - mu*nu)
  out
}
