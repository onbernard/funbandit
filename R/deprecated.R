

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

rewards_from_matrix <- function(reward_data) {
  reward_data <- as.matrix(reward_data)

  function(t, choice) {
    r <- reward_data[t, choice$which]
    list(arm=choice$which, reward=r)
  }
}

info_regret <- function(reward_data) {
  reward_data <- as.matrix(reward_data)
  cumulative_regret <- 0

  function(t, choice, reward) {
    arm <- reward$arm
    r <- reward$reward
    cumulative_regret <<- cumulative_regret + max(reward_data[t,]) - reward_data[t, arm]
    c(choice, reward, list(cum_regret=cumulative_regret))
  }
}

#' @export
aggregate_agent <- function(policy, PolArgs = list()) {
  UseMethod("aggregate_agent")
}

#' @export
aggregate_agent.policy <- function(policy, PolArgs = NULL) {
  force(policy)
  force(PolArgs)


  Mu <- NULL
  Nu <- NULL
  cum_regret <- 0
  t <- 1

  agent <- NULL
  k <- -1

  structure(function(reward_data) {
    rewardmat <- prepare_reward_data(reward_data)
    if (k == -1) {
      k <<- ncol(rewardmat)
    }
    stopifnot(k == ncol(rewardmat))
    if (is.null(agent)) {
      if (is.null(PolArgs)) {
        agent <<- policy(k=k)
      }
      else {
        agent <<- do.call(policy, PolArgs)
      }
      Mu <<- matrix(0, nrow = 1, ncol = k)
      Nu <<- matrix(0, nrow = 1, ncol = k)
    }

    h <- nrow(rewardmat)

    whatnext <- data.frame(agent$choose(), stringsAsFactors = F)
    choice <- whatnext$which
    agent$receive(choice, rewardmat[1, choice])
    regret <- max(rewardmat[1, ]) - rewardmat[1, choice]
    cum_regret <<- cum_regret + regret
    myrow <-
      cbind(
        whatnext,
        expectation = Mu[choice],
        reward = rewardmat[1, choice],
        cum_regret = cum_regret,
        t = t
      )
    if (Nu[choice] == 0) {
      Mu[choice] <<- rewardmat[1, choice]
    }
    else {
      Mu[choice] <<-
        ((Mu[choice] * Nu[choice] + rewardmat[1, choice]) / (Nu[choice] + 1))
    }
    Nu[choice] <<- Nu[choice] + 1
    t <<- t + 1

    d <- myrow
    nCol <- ncol(d)
    if (h > 1) {
      d <- d[rep.int(1, h), ]
      d <- as.list(d) # hack
      for (i in seq.int(2, h, 1)) {
        whatnext <- data.frame(agent$choose(), stringsAsFactors = F)
        choice <- whatnext$which
        agent$receive(choice, rewardmat[i, choice])
        regret <- max(rewardmat[i, ]) - rewardmat[i, choice]
        cum_regret <<- cum_regret + regret
        myrow <-
          cbind(
            whatnext,
            expectation = Mu[choice],
            reward = rewardmat[i, choice],
            cum_regret = cum_regret,
            t = t,
            StringsAsFactors = F
          )
        if (Nu[choice] == 0) {
          Mu[choice] <<- rewardmat[i, choice]
        }
        else {
          Mu[choice] <<-
            ((Mu[choice] * Nu[choice] + rewardmat[i, choice]) / (Nu[choice] + 1))
        }
        Nu[choice] <<- Nu[choice] + 1
        t <<- t + 1

        for (j in seq_len(nCol)) {
          d[[j]][i] <- myrow[[j]]
        }
      }
    }
    df <- data.frame(d, stringsAsFactors = FALSE)
    df$policy <- attributes(agent)$agent_name
    df$policy <- factor(df$policy)
    df
  }, class = c("aggregated_agent"))
}

#' @export
aggregate_agent.contextual_policy <-
  function(policy, PolArgs = list()) {
    force(policy)
    force(PolArgs)

    pol_name <- deparse1(substitute(policy))
    arg_name <-
      paste(mapply(function(n, v) {
        paste(list(n, v), collapse = "=")
      }, names(PolArgs), PolArgs), collapse = " ")
    pol_name <- paste(c(pol_name, "(", arg_name, ")"), collapse = "")


    Mu <- NULL
    Nu <- NULL
    cum_regret <- 0
    t <- 1

    agent <- NULL
    k <- -1
    dim <- -1

    structure(function(reward_data, context_data) {
      rewardmat <- prepare_reward_data(reward_data)
      contextmat <- prepare_reward_data(context_data)
      if (k == -1) {
        k <<- ncol(rewardmat)
      }
      stopifnot(k == ncol(rewardmat))
      if (dim == -1) {
        dim <<- ncol(contextmat)
      }
      stopifnot(dim == ncol(contextmat))
      stopifnot(nrow(rewardmat) == nrow(contextmat))
      if (is.null(agent)) {
        agent <<- do.call(policy, c(list(k = k, dim = dim), PolArgs))
        Mu <<- matrix(0, nrow = 1, ncol = k)
        Nu <<- matrix(0, nrow = 1, ncol = k)
      }
      h <- nrow(rewardmat)


      whatnext <-
        data.frame(agent$choose(contextmat[1, ]), stringsAsFactors = F)
      choice <- whatnext$which
      agent$receive(choice, rewardmat[1, choice], contextmat[1, ])
      regret <- max(rewardmat[1, ]) - rewardmat[1, choice]
      cum_regret <<- cum_regret + regret
      myrow <-
        cbind(
          whatnext,
          expectation = Mu[choice],
          reward = rewardmat[1, choice],
          cum_regret = cum_regret,
          t = t
        )
      if (Nu[choice] == 0) {
        Mu[choice] <<- rewardmat[1, choice]
      }
      else {
        Mu[choice] <<-
          ((Mu[choice] * Nu[choice] + rewardmat[1, choice]) / (Nu[choice] + 1))
      }
      Nu[choice] <<- Nu[choice] + 1
      t <<- t + 1

      d <- myrow
      nCol <- ncol(d)
      if (h > 1) {
        d <- d[rep.int(1, h), ]
        d <- as.list(d) # hack
        for (i in seq.int(2, h, 1)) {
          whatnext <-
            data.frame(agent$choose(contextmat[i, ]), stringsAsFactors = F)
          choice <- whatnext$which
          agent$receive(choice, rewardmat[i, choice], contextmat[i, ])
          regret <- max(rewardmat[i, ]) - rewardmat[i, choice]
          cum_regret <<- cum_regret + regret
          myrow <-
            cbind(
              whatnext,
              expectation = Mu[choice],
              reward = rewardmat[i, choice],
              cum_regret = cum_regret,
              t = t
            )
          if (Nu[choice] == 0) {
            Mu[choice] <<- rewardmat[i, choice]
          }
          else {
            Mu[choice] <<-
              ((Mu[choice] * Nu[choice] + rewardmat[i, choice]) / (Nu[choice] + 1))
          }
          Nu[choice] <<- Nu[choice] + 1
          t <<- t + 1

          for (j in seq_len(nCol)) {
            d[[j]][i] <- myrow[[j]]
          }
        }
      }
      df <- data.frame(d, stringsAsFactors = FALSE)
      df$policy <- pol_name
      df$policy <- factor(df$policy)
      df
    },
    class = c("aggregated_contextual_agent", "aggregated_agent"))
  }

run_policy <- function(policy, PolArgs = NULL, X, getreward, getanalyze) {

  h <- length(X)
  agent <- do.call(policy, PolArgs)

  choice <- agent$choose()
  reward <- getreward(X[1], choice)
  analysis <- getanalyze(X[1], choice, reward)
  do.call(agent$receive, reward)
  myrow <-
    data.frame(
      t=X[1],
      analysis,
      stringsAsFactors = F
    )

  d <- myrow
  nCol <- ncol(d)
  if (h > 1) {
    d <- d[rep.int(1, h), ]
    d <- as.list(d) # hack
    for (i in seq_along(X)) {
      choice <- agent$choose()
      reward <- getreward(X[i], choice)
      print("lol2")
      do.call(agent$receive, reward)
      myrow <-
        data.frame(
          t=X[i],
          analysis,
          stringsAsFactors = F
        )

      for (j in seq_len(nCol)) {
        d[[j]][i] <- myrow[[j]]
      }
    }
  }
  df <- data.frame(d, stringsAsFactors = FALSE)
  df$policy <- attributes(agent)$name
  df$policy <- factor(df$policy)
  df
}
