exp3 <- function(k, gamma = 0.05) {
  force(k)
  force(gamma)

  weights <- rep(1, times=k)
  prob <- rep(0, k)
  last_reward <- 0
  estimated_reward <- Inf
  t <- 1

  whatnext <- function() {
    if (t <= k) {
      list(which=t, proba=estimated_reward)
    }
    else {
      which <- sample(1:k, size=1, replace=TRUE, prob = prob)
      list(which=which, proba=estimated_reward)
    }
  }

  nowwhat <- function(arm, reward) {
    last_reward <<- reward
    prob <<- mapply(expp, weights, MoreArgs = list(sum(weights), k, gamma))
    estimated_reward <<- last_reward/prob[arm]
    weights[arm] <<- weights[arm] * exp(gamma * estimated_reward / k)
    t <<- t + 1
  }

  list(whatnext=whatnext, nowwhat=nowwhat)
}

distr <- function(weights, gamma) {
  k <- length(weights)
  prob <- rep(0, k)
  sumweights <- sum(weights)
  for (a in 1:k) {
    prob[a] <- (1-gamma) * (weights[a]/sumweights) + (gamma/k)
  }
  prob
}

expp <- function(w, sum, k, gamma) {
  (1-gamma) * (w/sum) + (gamma/k)
}
