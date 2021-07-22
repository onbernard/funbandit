mkRound <- function(agent, rewards) {
  UseMethod("mkRound")
}

mkRound.agent <- function(agent, rewards) {
  force(agent)
  force(rewards)
  k <- nrow(rewards)

  Mu <- matrix(0, nrow = 1, ncol = k)
  Nu <- matrix(0, nrow = 1, ncol = k)
  cum_regret <- 0
  t <- 1

  function(t) {
    whatnext <- agent$choose()
    choice <- whatnext$which
    agent$receive(choice, rewards[t,choice])
    regret <- max(rewards[t,]) - rewards[t,choice]
    cum_regret <<- cum_regret + regret
    outp <- cbind(whatnext, expectation = Mu[choice], reward = rewards[t,choice], cum_regret=cum_regret, t=t)
    if (Nu[choice] == 0) {
      Mu[choice] <<- rewards[t,choice]
    }
    else {
      Mu[choice] <<-
        ((Mu[choice] * Nu[choice] + rewards[t,choice]) / (Nu[choice] + 1))
    }
    Nu[choice] <<- Nu[choice] + 1
    t <<- t+1
    outp
  }
}

apply_policy <- function(policy, rewards, PolicyArgs=NULL) {
  rewards <- as.matrix(rewards)
  pol <- c()
  name <- deparse1(substitute(policy))
  if (is.null(PolicyArgs)) {
    pol <- policy(k=ncol(rewards))
  }
  else {
    pol <- policy(k=ncol(rewards), unlist(PolicyArgs))
    name <- paste(name, deparse1(substitute(PolicyArgs)))
  }

  h <- nrow(rewards)
  k <- ncol(rewards)
  mkRow <- mkRound(pol, rewards)

  d <- mkRow(1)
  d <- d[rep.int(1,h),]
  nCol <- ncol(d)
  if (h>1) {
    d <- d[rep.int(1,h),]
    d <- as.list(d)
    for (i in seq.int(2,h,1)) {
      ri <- mkRow(i)
      for (j in seq_len(nCol)) {
        d[[j]][i] <- ri[[j]]
      }
    }
  }
  df <- data.frame(d, stringsAsFactors = FALSE)
  df$pol <- name
  df$pol <- factor(df$pol)
  df
}

# data <- gen_data(10000)
# ucb <- apply_policy(upper_confidence_bound, data)
# ts <- apply_policy(thompson_sampling, data)
# epsg <- apply_policy(epsilon_greedy, data)
# results <- list(ucb=ucb,ts=ts,epsg=epsg)
# compare_regrets(results)
compare_results <- function(results) {
  df <-
    cbind(cat = rep(names(results), sapply(results, nrow)), do.call(bind_rows, results))
  regretplt <-
    ggplot(data = df, aes(x = t, y = cum_regret, color = cat)) + geom_line()
  choicehistplt <-
    ggplot(df, aes(x = which, group = pol, fill = pol)) + geom_histogram(position =
                                                                           "dodge", binwidth = 0.25) + theme_bw()
  ggarrange(regretplt, choicehistplt)
}
