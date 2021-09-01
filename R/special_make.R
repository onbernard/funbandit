


#' @export
deprecated_make_policy <- function(init, choose, receive, name) {
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
    # ====================
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
    # ====================
    outp <- structure(
      list(
        choose = structure(choose, class = "agent_choose"),
        receive = structure(receive, class = "agent_receive")
      ),
      agent_name = paste(c(name, " (", argstring, ")"), collapse=""),
      k = k,
      class = c("agent")
    )


    attributes(outp)$agent_arguments <- args

    environment(outp$choose) <- e
    environment(outp$receive) <- e
    outp
  }
  ,
  policy_name = name,
  class = c("policy")))
}
