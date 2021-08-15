#'  Exponential-weight algorithm for Exploration and Exploitation Policy
#'
#' @param k number of arms
#' @param gamma exploration parameter
#'
#' @return An agent object, i.e. a list of two functions : choose and receive
#' @export
#'
#' @examples
exp3 <- structure(function(k, gamma = 0.05) {
  k <- as.integer(k)
  gamma <- as.double(gamma)

  weights <- rep(1, times=k)
  prob <- rep(0, k)
  last_reward <- 0
  estimated_reward <- Inf
  t <- 1

  choose <- structure(function() {
    if (t <= k) {
      data.frame(which=t, estimated_reward=estimated_reward, stringsAsFactors = FALSE)
    }
    else {
      which <- sample(1:k, size=1, replace=TRUE, prob = prob)
      data.frame(which=which, estimated_reward=estimated_reward, stringsAsFactors = FALSE)
    }
  }, class="agent.choose")

  receive <- structure(function(arm, reward) {
    last_reward <<- reward
    prob <<- mapply(expp, weights, MoreArgs = list(sum(weights), k, gamma))
    estimated_reward <<- last_reward/prob[arm]
    weights[arm] <<- weights[arm] * exp(gamma * estimated_reward / k)
    t <<- t + 1
  }, class="agent.receive")

  structure(list(choose=choose, receive=receive), k=k, class="agent")
}, class="policy")


#' EXP3 expectation estimation of an arm
#'
#' @param w arm's weight
#' @param sum sum of all arm weights
#' @param k number of arms
#' @param gamma exploration parameter
#'
#' @return
#' @export
#'
#' @examples
expp <- function(w, sum, k, gamma) {
  (1-gamma) * (w/sum) + (gamma/k)
}
