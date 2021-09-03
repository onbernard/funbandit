

#' @export
funmake <- function(init, choose, receive, name, argnames) {
  force(init)
  force(choose)
  force(receive)
  name <- as.character(name)
  argnames <- as.character(argnames)

  function(...) {
    PolArgs <- list(...)
    force(PolArgs)

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


    e <- new.env()
    this.choose <- choose
    this.receive <- receive
    environment(this.choose) <- e
    environment(this.receive) <- e

    # =====================


    # =======================
    cum_regret <- 0
    t <- 1

    k <- -1
    # ====================

    wtf <- function(reward_data) {
      rewardmat <- prepare_reward_data(reward_data)
      if (k==-1) {
        k <<- ncol(rewardmat)
        # === Create and fill agent environment
        model <- do.call(init, c(k, PolArgs))
        if (typeof(model) != "list") {
          rlang::abort("A policy initiator must return a named list.")
        }
        if (length(model) != sum(nzchar(names(model)))) {
          rlang::abort("A policy initiator must return a named list.")
        }

        list2env(model, envir = e)
        # ==========
      }

      stopifnot(k == ncol(rewardmat))
      h <- nrow(rewardmat)

      whatnext <- data.frame(this.choose(), stringsAsFactors = F)
      choice <- whatnext$which
      this.receive(choice, rewardmat[1, choice])
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
      if (h > 1) {
        d <- d[rep.int(1, h),]
        d <- as.list(d) # hack
        for (i in seq.int(2, h, 1)) {
          whatnext <- data.frame(this.choose(), stringsAsFactors = F)
          choice <- whatnext$which
          this.receive(choice, rewardmat[i, choice])
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

    # ===================================================
    structure(
      wtf,
      choose = this.choose,
      receive = this.receive,
      policy_name = name,
      agent_name = agent_name,
      arguments = arguments
    )
  }
}
