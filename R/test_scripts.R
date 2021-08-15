library(devtools)
library(tidyverse)
library(sloop)
library(ggpubr)
library(microbenchmark)
library(ggplot2)

gen_rewardmat <- function(h, k, prob=runif(1)) {
  mat <- matrix(NA, ncol=k, nrow=h)
  for(a in 1:k) {
    mat[,a] <- rbinom(h, 1, prob)
  }
  mat
}

test_con <- function() {
  size.tot = 1000                        # this makes the example exactly reproducible
  x1 = runif(size.tot, min=0, max=10)          # you have 4, largely uncorrelated predictors
  x2 = runif(size.tot, min=0, max=10)
  x3 = runif(size.tot, min=0, max=10)
  x4 = runif(size.tot, min=0, max=10)
  dt = cbind(x1,x2,x3,x4)
  #arm reward
  arm_1 <-  as.vector(c(-1,9,-8,4))
  K1 = crossprod(t(dt),arm_1)
  arm_2 <-  as.vector(c(-1,2,1,0))
  K2 = crossprod(t(dt),arm_2)
  arm_3 <-  as.vector(c(-1,-5,1,10))
  K3 = crossprod(t(dt),arm_3)
  visitor_reward <-  cbind(K1,K2,K3)

  linucb <- linear_upper_confidence_bound(3, 4)
  lints  <- linear_thompson_sampling(3,4)
  policies <- list(linucb, lints)

  lapply(policies, function(pol){
    choices <- rep.int(0,1000)
    for(i in 1:1000) {
      choices[i] <- pol$choose(dt[i,])$which
      pol$receive(choices[i], visitor_reward[i, choices[i]], dt[i,])
    }
    choices
  })
}

