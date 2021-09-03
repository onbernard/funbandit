
#' @export
make_policy <- function(init, choose, receive, name, argnames) {
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

    this.choose <- choose
    this.receive <- receive
    environment(this.choose) <- e
    environment(this.receive) <- e
    # ====================

    # ====================
    structure(
      list(choose=this.choose, receive=this.receive),
      policy_name=name,
      agent_name = paste(c(name, " (", paste(names(arguments), arguments, sep = "=", collapse=" "), ")"), collapse=""),
      arguments = arguments,
      class="agent"
    )
  }, class="policy")
}


