
#' @export
make_policy <- function(init, choose, receive, name) {
  list(init, choose, receive, name)
  force(init)
  force(choose)
  force(receive)
  name <- as.character(name)


  return(structure(function(k, PolArgs = eval(formals(init)$PolArgs)) {
    force(k)
    lapply(PolArgs, force)

    # === Create and fill agent environment
    e <- new.env()
    model_variables <- init(k, PolArgs)
    for (i in seq_along(model_variables)) {
      e[[eval(names(model_variables)[[i]])]] <- model_variables[[i]]
    }

    outp <- structure(
      list(
        choose = structure(choose, class = "agent_choose"),
        receive = structure(receive, class = "agent_receive")
      ),
      policy_name = name,
      k = k,
      class = c("agent")
    )
    args <- c()
    if (length(PolArgs) == 0) {
      args <- eval(formals(init)$PolArgs)
    }
    else {
      args <- PolArgs
    }
    argstring <-
      paste(mapply(function(n, v) {
        paste(list(n, v), collapse = "=")
      }, names(args), args), collapse = " ")

    attributes(outp)$policy_args <- argstring

    environment(outp$choose) <- e
    environment(outp$receive) <- e
    outp
  }
  ,
  name = name,
  class = c("policy")))
}

