make_policy <- function(init, choose, receive, name) {
  force(init)
  force(choose)
  force(receive)
  name <- as.character(name)

  structure(function(k, PolArgs=eval(formals(init)$PolArgs)) {
    force(k)
    lapply(PolArgs, force)

    # === Create and fill agent environment
    e <- new.env()
    model_variables <- init(k, PolArgs)
    for (ii in seq_along(model_variables)) {
      e[[eval(names(model_variables)[[ii]])]] <- model_variables[[ii]]
    }

    outp <- structure(
      list(
        choose = structure(choose, class = "agent_choose"),
        receive = structure(receive, class = "agent_receive")
      ),
      PolName = name,
      class = "agent"
    )
    args <- c()
    if (length(PolArgs)==0) {
      args <- eval(formals(init)$PolArgs)
    }
    else {
      args <- PolArgs
    }
    argstring <- paste(mapply(function(n, v){paste(list(n,v), collapse="=")}, names(args), args), collapse=" ")

    attributes(outp)$PolArgs <- argstring

    environment(outp$choose) <- e
    environment(outp$receive) <- e
    print(ls(e))
    outp
  }
  , name=name, class = "policy")
}
