run_policy <- function(policy, PolArgs = NULL, X, reward, analyze) {

  h <- length(X)
  agent <- aggregate_agent(policy, PolArgs)

  choice <- agent$choose()
  reward <- reward(X[1], choice)
  additional_infos <- analyze(X[1], choice, reward)
  agent$receive(whatnext)
  myrow <-
    data.frame(
      t=X[1],
      analyze,
      stringsAsFactors = F
    )

  d <- myrow
  nCol <- ncol(d)
  if (h > 1) {
    d <- d[rep.int(1, h), ]
    d <- as.list(d) # hack
    for (i in seq_along(X)) {
      choice <- agent$choose()
      reward <- reward(X[i], choice)
      additional_infos <- analyze(X[i], choice, reward)
      agent$receive(whatnext)
      myrow <-
        data.frame(
          t=X[i],
          analyze,
          stringsAsFactors = F
        )

      for (j in seq_len(nCol)) {
        d[[j]][i] <- myrow[[j]]
      }
    }
  }
  df <- data.frame(d, stringsAsFactors = FALSE)
  df$policy <- pol_name
  df$policy <- factor(df$policy)
  df
}
