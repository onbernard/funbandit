#' Upper Confidence Bound Policy
#'
#' @param k number of arms
#' @param alpha exploration parameter
#'
#' @return An agent object, i.e. a list of two functions : choose and receive
#' @export
#'
#' @examples
upper_confidence_bound <- structure(function(k, alpha=1) {
  alpha <- as.double(alpha)
  k <- as.integer(k)

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

  structure(list(choose=choose, receive=receive), k=k, class="agent")
}, class="policy")


#' Upper Confidence Bound value of an arm
#'
#' @param mu observed mean
#' @param nu number of trials
#' @param alpha exploration parameter
#' @param t iteration index
#'
#' @return double value
#' @export
#'
#' @examples
ucb <- function(mu, nu, alpha, t) {
  if (nu == 0) {
    Inf
  }
  else {
    mu + alpha * sqrt((2 * log(t)) / nu)
  }
}
