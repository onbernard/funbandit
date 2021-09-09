#' @export
make_aio <- function(init, choose, receive, name, argnames) {
  force(init)
  force(choose)
  force(receive)
  name <- as.character(name)
  argnames <- as.character(argnames)

  structure(function(...) {
    # === === === === ===
    PolArgs <- list(...)
    force(PolArgs)
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
    e <- new.env()
    environment(this.choose) <- e
    environment(this.receive) <- e
    # === === === === ===
    cum_regret <- 0
    t <- 1
    k <- -1
    # === === === === ===
    all_in_one <- function(reward_data) {
      rewardmat <- prepare_reward_data(reward_data)
      if (k == -1) {
        k <<- ncol(rewardmat)
        pol2env(this.init, c(list(k = k), PolArgs), e)
      }
      stopifnot(k == ncol(rewardmat))
      h <- nrow(rewardmat)
      whatnext <- data.frame(this.choose(), stringsAsFactors = F)
      choice <- whatnext$which
      this.receive(choice, rewardmat[1, choice])
      regret <- max(rewardmat[1,]) - rewardmat[1, choice]
      cum_regret <<- cum_regret + regret
      myrow <-
        c(whatnext,
          reward = rewardmat[1, choice],
          regret = regret,
          cum_regret = cum_regret)
      t <<- t + 1
      dt <- as.data.table(c(myrow, list(t = 1)))
      if (h > 1) {
        dt <- as.data.table(c(myrow, list(t = seq.int(1, h, 1))))
        for (i in seq.int(2, h, 1)) {
          whatnext <- this.choose()
          choice <- whatnext$which
          this.receive(choice, rewardmat[i, choice])
          regret <- max(rewardmat[i,]) - rewardmat[i, choice]
          cum_regret <<- cum_regret + regret
          myrow <-
            c(whatnext,
              reward = rewardmat[i, choice],
              regret = regret,
              cum_regret = cum_regret)
          t <<- t + 1
          dt[i, names(myrow) := myrow]
        }
      }
      dt[,"policy" := .(factor(agent_name))][]
      dt
    }
    # === === === === ===
    structure(
      all_in_one,
      policy_name = name,
      agent_name = agent_name,
      arguments = arguments,
      class = c("aio", "agent")
    )
  },
  policy_name = name,
  class = c("aio", "policy"))
}
