cyclical <- structure(function(k) {
  k <- as.integer(k)

  Mu <- matrix(Inf, nrow = 1, ncol = k)
  Nu <- matrix(0, nrow = 1, ncol = k)
  which <- 1

  choose <- structure(function() {
    tibble(which=which, proba=Mu[which])
  }, class="agent.choose")

  receive <- structure(function(arm, reward) {
    if (Nu[arm] == 0) {
      Mu[arm] <<- reward
    }
    else {
      Mu[arm] <<- ((Mu[arm] * Nu[arm] + reward) / (Nu[arm] + 1))
    }
    Nu[arm] <<- Nu[arm] + 1
    which <<- which%%k + 1
  }, class="agent.receive")

  structure(list(whatnext=whatnext, nowwhat=nowwhat), k=k, name="cyclical", class="agent")
}, class="policy")
