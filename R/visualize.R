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


compare_results <- function(results) {
  df <- do.call(bind_rows, results)
  #cbind(ww = rep(names(results), sapply(results, nrow)), do.call(bind_rows, results))
  # labels = function(x) {str_wrap(x, width = 2) }
  df$policy <- stringr::str_wrap(df$policy, width = 6)
  regretplt <-
    ggplot(data = df, aes(x = t, y = cum_regret, color = policy)) +
    scale_color_viridis_d() +
    geom_smooth(size = 1.6, se = F) + geom_point(size = 1.5)


  cir <-
    ggplot(df, aes(policy, fill = factor(which))) +
    ggsci::scale_fill_jco() +
    geom_bar() + theme(axis.text.x = element_text(angle = -90, vjust = 0.5)) +
    labs(fill="arm")

  propplt <-
    ggplot(df, aes(fill = policy)) +
    scale_fill_viridis_d() +
    geom_bar(aes(x=which, y=..prop..), position="dodge") +
    scale_y_continuous(labels = scales::percent) +
    labs(x="arm")

  choiceplt <-
    ggplot(df, aes(x=t, y=factor(which))) +
    geom_step(aes(color=policy, group=1)) + facet_grid(policy ~ .) +
    theme(legend.position = "none") + theme(strip.text.y = element_text(angle = 0)) +
    scale_color_viridis_d() +
    labs(y="arm")

  #ggpubr::ggarrange(regretplt, propplt, choiceplt, cir)
  gridExtra::grid.arrange(choiceplt,
               gridExtra::arrangeGrob(cir, propplt, ncol=2),
               regretplt,
               nrow=3)
}

# TODO : add arm mean to hist
# plot_hist <- function(results, rewardmat) {
#   choicehistplt <-
#     ggplot(df, aes(x = which, group = policy, fill = policy)) + geom_histogram(position =
#                                                                                  "dodge", binwidth = 0.25) + theme_bw()
#   choicehistplt
# }

plot_regret <- function(aggregated_agent, reward_data) {
  df <- aggregated_agent(reward_data)
  ggplot(data = df, aes(x = t, y = cum_regret, color = agent)) +
    scale_colour_discrete(labels=function(x){str_wrap(x, width = 2)}) +
    geom_smooth(size=1.6, se=F) + geom_point(size=1.5)
}

average <-
  function(pol,
           iter,
           k,
           h,
           probmat = matrix(runif(k * iter),ncol=k),
           nrow = iter,
           ncol = k) {
    agent <- stat_agent(pol)
    rewardmat <- gen_rewardmat(h, k, probmat[1, ])
    d <- agent(rewardmat)
    d$iter <- 1
    for (i in seq.int(2, iter, 1)) {
      agent <- stat_agent(pol)
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
