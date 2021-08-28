
#' @export
special_make_policy <- function(init, choose, receive, name) {
  list(init, choose, receive, name)
  force(init)
  force(choose)
  force(receive)
  name <- as.character(name)


  return(structure(function(...) {
    PolArgs <- list(...)

    # === Create and fill agent environment
    e <- new.env()
    model_variables <- do.call(init, PolArgs)

    for (i in seq_along(model_variables)) {
      e[[eval(names(model_variables)[[i]])]] <- model_variables[[i]]
    }
    # ====================

    # ====================
    outp <- structure(
      list(
        choose = choose,
        receive = receive
      ),
      name=name,
      class = c("agent")
    )

    environment(outp$choose) <- e
    environment(outp$receive) <- e
    outp
  }
  ,
  name = name,
  class = c("policy")))
}

special_init <- function(k, epsilon = 0.25) {
  epsilon <- as.double(epsilon)
  k <- as.integer(k)

  Mu <- rep(Inf, k)
  Nu <- rep.int(0, k)
  t <- 1
  list(
    epsilon = epsilon,
    k = k,
    Mu = Mu,
    Nu = Nu,
    t = t
  )
}

special_eps <-
  special_make_policy(
    special_init,
    choose_epsilon_greedy,
    receive_epsilon_greedy,
    "special_eps"
  )
