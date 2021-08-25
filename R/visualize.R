plotTimings <- function(timings) {
  df <- cbind(sapply(timings, nrow), do.call(rbind, timings))

  df$expr <- reorder(df$expr, -df$time, FUN = max)
  ggplot(data = df, aes(x = nRow, y = time, color = expr)) +
    geom_point(alpha = 0.8) + geom_smooth(alpha = 0.8)
  nmax <- max(df$nRow)
  tsub <- df[df$nRow == nmax, ]
  tsub$expr <- reorder(tsub$expr, tsub$time, FUN = median)
  plt <- list(
    ggplot(data = df, aes(
      x = nRow, y = time, color = expr
    )) +
      geom_point(alpha = 0.8) + geom_smooth(alpha = 0.8),
    ggplot(data = df, aes(
      x = nRow, y = time, color = expr
    )) +
      geom_point(alpha = 0.8) + geom_smooth(alpha = 0.8) +
      scale_y_log10(),
    WVPlots::ScatterBoxPlot(tsub, 'expr', 'time',
                            title = paste('nRow = ', nmax)) +
      coord_flip()
  )
  do.call(ggarrange, plt)
}


timings <- function(FUN, timeSeq) {
  outp <- vector("list", length(timeSeq))
  for (i in seq_len(length(timeSeq))) {
    nRow <- timeSeq[[i]]
    ti <- microbenchmark(FUN(i),
                         times = 10)
    ti <- data.frame(ti, stringsAsFactors = FALSE)
    ti$nRow <- nRow
    ti$nCol <- 5
    outp[[i]] <- ti
  }
  outp <- data.table::rbindlist(outp)
  outp$expr <- deparse1(substitute(FUN))
  outp
}

# data <- gen_data(10000)
# ucb <- gobble(upper_confidence_bound(5L))(data)
# ts <- gobble(thompson_sampling(5L))(data)
# epsg <- gobble(epsilon_greedy(5L))(data)
# results <- list(ucb=ucb,ts=ts,epsg=epsg)
# compare_results(results)
compare_results <- function(results) {
  df <- do.call(bind_rows, results)
    #cbind(ww = rep(names(results), sapply(results, nrow)), do.call(bind_rows, results))
  regretplt <-
    ggplot(data = df, aes(x = t, y = cum_regret, color = policy)) + geom_line()
  choicehistplt <-
    ggplot(df, aes(x = which, group = policy, fill = policy)) + geom_histogram(position =
                                                                               "dodge", binwidth = 0.25) + theme_bw()
  ggpubr::ggarrange(regretplt, choicehistplt)
}


average <-
  function(pol,
           iter,
           k,
           h,
           probmat = matrix(runif(k * iter),ncol=k),
           nrow = iter,
           ncol = k) {
    agent <- stat_policy(pol)
    rewardmat <- gen_rewardmat(h, k, probmat[1, ])
    d <- agent(rewardmat)
    d$iter <- 1
    for (i in seq.int(2, iter, 1)) {
      agent <- stat_policy(pol)
      rewardmat <- gen_rewardmat(h, k, probmat[i,])
      dp <- agent(rewardmat)
      dp$iter <- i
      d <- rbind(d, dp)
    }
    d$iter <- as_factor(d$iter)
    print(ggplot(data = d, aes(x = t, y = cum_regret, color = iter)) +
      geom_point(alpha = 0.8) + geom_smooth(alpha = 0.8, color = "black"))
    return(list(probmat=probmat, results=d))
  }
