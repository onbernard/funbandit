make_policy <- function(init, choose, receive) {
  structure(function(k, ...) {

    e <- new.env()

    model_variables <- init(k, ...)
    for (ii in seq_along(model_variables)) {
      e[[eval(names(model_variables)[[ii]])]] <- model_variables[[ii]]
    }

    outp <- structure(list(
      choose = structure(choose, class = "agent.choose"),
      receive = structure(receive, class = "agent.receive")), class="agent")

    environment(outp$choose) <- e
    environment(outp$receive) <- e

    outp
  }
  , class = "policy")
}

test_choose <- function() {
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
}

test_receive <- function(arm, reward) {
  if (Nu[arm] == 0) {
    Mu[arm] <<- reward
  }
  else {
    Mu[arm] <<- ((Mu[arm] * Nu[arm] + reward) / (Nu[arm] + 1))
  }
  Nu[arm] <<- Nu[arm] + 1
  t <<- t + 1
}

test_init <- function(k, epsilon=0.25) {
  epsilon <- as.double(epsilon)
  k <- as.integer(k)

  Mu <- matrix(Inf, nrow = 1, ncol = k)
  Nu <- matrix(0, nrow = 1, ncol = k)
  t <- 1
  list(epsilon=epsilon, k=k,Mu=Mu,Nu=Nu,t=t)
}

test_pol <- make_policy(test_init, test_choose, test_receive)
