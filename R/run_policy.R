run_policy <- function(policy, PolArgs = NULL, X, reward, analyze) {

  h <- length(X)
  agent <- do.call(policy, PolArgs)

  choice <- agent$choose()
  reward <- reward(X[1], choice)
  analysis <- analyze(X[1], choice, reward)
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
      reward <- reward(X[i], choice)
      analysis <- analyze(X[i], choice, reward)
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
