agent_attributes <- function(init, PolArgs, name, argnames) {

  if (length(PolArgs) != sum(nzchar(names(PolArgs)))) {
    rlang::abort("All arguments must be named.")
  }

  arguments <- formals(init)
  for (i in seq_along(PolArgs)) {
    arguments[[eval(names(PolArgs)[[i]])]] <- PolArgs[[i]]
  }
  arguments <- arguments[argnames]

  agent_name <- paste(c(
    name,
    " (",
    paste(
      names(arguments),
      arguments,
      sep = "=",
      collapse = " "
    ),
    ")"
  ), collapse = "")

  list(arguments=arguments, agent_name=agent_name)
}
