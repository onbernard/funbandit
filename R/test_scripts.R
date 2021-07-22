library(devtools)
library(tidyverse)
library(sloop)
library(ggpubr)
library(microbenchmark)


plotTimings <- function(timings) {
  df <- cbind(sapply(timings,nrow), do.call(rbind,timings))

  df$expr <- reorder(df$expr,-df$time,FUN=max)
  ggplot(data=df,aes(x=nRow,y=time,color=expr)) +
    geom_point(alpha=0.8) + geom_smooth(alpha=0.8)
  nmax <- max(df$nRow)
  tsub <- df[df$nRow==nmax,]
  tsub$expr <- reorder(tsub$expr,tsub$time,FUN=median)
  plt <- list(
    ggplot(data=df,aes(x=nRow,y=time,color=expr)) +
      geom_point(alpha=0.8) + geom_smooth(alpha=0.8),
    ggplot(data=df,aes(x=nRow,y=time,color=expr)) +
      geom_point(alpha=0.8) + geom_smooth(alpha=0.8) +
      scale_y_log10(),
    WVPlots::ScatterBoxPlot(tsub,'expr','time',
                            title=paste('nRow = ',nmax)) +
      coord_flip()
  )
  do.call(ggarrange, plt)
}


timings <- function(FUN, timeSeq) {
  outp <- vector("list", length(timeSeq))
  for(i in seq_len(length(timeSeq))) {
    nRow <- timeSeq[[i]]
    ti <- microbenchmark(FUN(i),
                         times=10)
    ti <- data.frame(ti, stringsAsFactors = FALSE)
    ti$nRow <- nRow
    ti$nCol <- 5
    outp[[i]] <- ti
  }
  outp <- data.table::rbindlist(outp)
  outp$expr <- deparse1(substitute(FUN))
  outp
}

