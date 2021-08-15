#' Fixed arm pull probabilities Policy
#'
#' @param k number of arms
#' @param prob pull probability of each arm
#'
#' @return An agent object, i.e. a list of two functions : choose and receive
#' @export
#'
#' @examples
from_distribution <- structure(function(k, prob) {
  k <- as.integer(k)
  force(prob)

  choose <- structure(function() {
    which <- sample(seq.int(1,k,1), 1, prob = prob)
    tibble(which=which, p=prob[which])
  }, class="agent.choose")

  receive <- structure(function(arm, reward) {}, class="agent.receive")

  structure(list(choose=choose, receive=receive), k=k, class="agent")
}, class="policy")
