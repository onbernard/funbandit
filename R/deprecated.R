

funmake <- function(init, choose, receive, name, argnames) {
  force(init)
  force(choose)
  force(receive)
  name <- as.character(name)
  argnames <- as.character(argnames)

  structure(function(...) {
    PolArgs <- list(...)

    p <- parent.env(environment())
    init <- p$init
    choose <- p$choose
    receive <- p$receive
    name <- p$name
    argnames <- p$argnames

    if (length(PolArgs) != sum(nzchar(names(PolArgs)))) {
      rlang::abort("All arguments must be named.")
    }

    arguments <- formals(init)
    for ( i in seq_along(PolArgs)) {
      arguments[[eval(names(PolArgs)[[i]])]] <- PolArgs[[i]]
    }
    arguments <- arguments[argnames]


    bandit <- do.call(init, PolArgs)

    structure(
      bandit,
      choose=choose,
      receive=receive,
      agent_name = paste(c(name, " (", paste(names(arguments), arguments, sep = "=", collapse=" "), ")"), collapse=""),
      policy_name = name,
      class="agent")
  },
  name=name,
  class="policy")
}




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
