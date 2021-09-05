#' @export
make_aio_ctxt <- function(init, choose, receive, name, argnames) {
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
    dim <- -1
    # === === === === ===
    all_in_one <- function(reward_data, context_data) {
      rewardmat <- prepare_reward_data(reward_data)
      contextmat <- prepare_reward_data(context_data)
      if (k==-1) {
        k <<- ncol(rewardmat)
        dim <<- ncol(contextmat)
        pol2env(this.init, c(list(k=k, dim=dim), PolArgs), e)
      }
      stopifnot(k == ncol(rewardmat))
      stopifnot(dim == ncol(contextmat))
      h <- nrow(rewardmat)
      # === === === === ===
      whatnext <- data.frame(this.choose(contextmat[1,]), stringsAsFactors = F)
      choice <- whatnext$which
      this.receive(choice, rewardmat[1, choice], contextmat[1,])
      regret <- max(rewardmat[1,]) - rewardmat[1, choice]
      cum_regret <<- cum_regret + regret
      myrow <-
        cbind(whatnext,
              reward = rewardmat[1, choice],
              cum_regret = cum_regret,
              t = t)
      t <<- t + 1
      d <- myrow
      nCol <- ncol(d)
      # === === === === ===
      if (h > 1) {
        d <- d[rep.int(1, h),]
        d <- as.list(d) # hack
        for (i in seq.int(2, h, 1)) {
          whatnext <- data.frame(this.choose(contextmat[t,]), stringsAsFactors = F)
          choice <- whatnext$which
          this.receive(choice, rewardmat[i, choice], contextmat[t,])
          regret <- max(rewardmat[i,]) - rewardmat[i, choice]
          cum_regret <<- cum_regret + regret
          myrow <-
            cbind(
              whatnext,
              reward = rewardmat[i, choice],
              cum_regret = cum_regret,
              t = t,
              StringsAsFactors = F
            )
          t <<- t + 1
          for (j in seq_len(nCol)) {
            d[[j]][i] <- myrow[[j]]
          }
        }
      }
      df <- data.frame(d, stringsAsFactors = FALSE)
      df$policy <- agent_name
      df$policy <- factor(df$policy)
      df
    }
    # === === === === ===
    structure(
      all_in_one,
      policy_name = name,
      agent_name = agent_name,
      arguments = arguments,
      class=c("ctxt_aio", "ctxt_agent")
    )
  }, policy_name = name, class = c("ctxt_aio", "ctxt_policy"))
}
