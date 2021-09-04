


init_exp3 <- function(k, gamma = 0.05) {
  k <- as.integer(k)
  gamma <- as.double(gamma)

  weights <- rep(1, k)
  prob <- rep(0, k)
  last_reward <- 0
  estimated_reward <- Inf
  t <- 1
  list(
    k = k,
    gamma = gamma,
    weights = weights,
    prob = prob,
    last_reward = last_reward,
    estimated_reward = estimated_reward,
    t = t
  )
}

choose_exp3 <- function() {
  if (t <= k) {
    list(which = t, estimated_reward = estimated_reward)
  }
  else {
    which <- sample(1:k,
                    size = 1,
                    replace = TRUE,
                    prob = prob)
    list(which = which, estimated_reward = estimated_reward)
  }
}

receive_exp3 <- function(arm, reward) {
  last_reward <<- reward
  prob <<-
    mapply(expp, weights, MoreArgs = list(sum(weights), k, gamma))
  estimated_reward <<- last_reward / prob[arm]
  weights[arm] <<- weights[arm] * exp(gamma * estimated_reward / k)
  t <<- t + 1
}

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
  (1 - gamma) * (w / sum) + (gamma / k)
}

