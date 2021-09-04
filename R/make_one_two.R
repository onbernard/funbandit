#' @export
make_one_two <- function(init, choose, receive, name, argnames) {
  force(init)
  force(choose)
  force(receive)
  name <- as.character(name)
  argnames <- as.character(argnames)
  # === === === === ===
  structure(function(...) {
    PolArgs <- list(...)
    # === === === === ===
    p <- parent.env(environment())
    this.init <- p$init
    this.choose <- p$choose
    this.receive <- p$receive
    name <- p$name
    argnames <- p$argnames
    # === === === === ===
    att <- agent_attributes(this.init, PolArgs, name, argnames)
    arguments <- att$arguments
    agent_name <- att$agent_name
    # === === === === ===
    e <- pol2env(init, PolArgs)
    environment(this.choose) <- e
    environment(this.receive) <- e
    # === === === === ===
    structure(
      list(
        choose = this.choose,
        receive = this.receive
      ),
      policy_name = name,
      agent_name = agent_name,
      arguments = arguments,
      class=c("one_two", "agent")
    )
  }, policy_name = name, class = c("one_two", "policy"))
}
