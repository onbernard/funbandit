epsilon_greedy <- structure(function(k, epsilon = 0.25) {
  epsilon <- as.double(epsilon)
  k <- as.integer(k)

  Mu <- matrix(Inf, nrow = 1, ncol = k)
  Nu <- matrix(0, nrow = 1, ncol = k)
  t <- 1

  choose <- structure(function() {
    if (t <= k) {
      data.frame(which = t, why="explore", stringsAsFactors = FALSE)
    }
    else {
      whatdo <- exploit_or_not(epsilon)
      which <- switch(
        whatdo,
        "exploit" = which.max(Mu),
        "explore" = sample(1:k, size = 1, replace = TRUE)
      )
      data.frame(which=which, why=whatdo, stringsAsFactors = FALSE)
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
    t <<- t + 1
  }, class="agent.receive")

  structure(list(choose=choose, receive=receive), k=k, class="agent")
}, class="policy")

exploit_or_not <- function(epsilon) {
  sample(
    c("exploit", "explore"),
    size = 1,
    replace = TRUE,
    prob = c(1 - epsilon, epsilon)
  )
}
