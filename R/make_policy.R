
#' @export
make_policy <- function(init, choose, receive, name) {
  force(init)
  force(choose)
  force(receive)
  name <- as.character(name)


  return(structure(function(...) {
    PolArgs <- list(...)

    if (length(PolArgs) != sum(nzchar(names(PolArgs)))) {
      rlang::abort("All arguments of a policy must be named.")
    }

    arguments <- formals(init)
    for ( i in seq_along(PolArgs)) {
      arguments[[eval(names(PolArgs)[[i]])]] <- PolArgs[[i]]
    }
    arguments$k <- NULL

    # === Create and fill agent environment
    e <- new.env()
    model <- do.call(init, c(PolArgs))
    if (typeof(model) != "list") {
      rlang::abort("A policy initiator must return a named list.")
    }
    if (length(model) != sum(nzchar(names(model)))) {
      rlang::abort("A policy initiator must return a named list.")
    }

    list2env(model, envir = e)
    # ====================

    # ====================
    outp <- structure(
      list(
        choose = choose,
        receive = receive
      ),
      policy_name=name,
      agent_name = paste(c(name, " (", paste(names(arguments), arguments, sep = "=", collapse=" "), ")"), collapse=""),
      arguments = arguments,
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


