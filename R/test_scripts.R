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

test_ctxt <- function(X) {
  size.tot <- 1000                        # this makes the example exactly reproducible
  x1 <- runif(size.tot, min=0, max=10)          # you have 4, largely uncorrelated predictors
  x2 <- runif(size.tot, min=0, max=10)
  x3 <- runif(size.tot, min=0, max=10)
  x4 <- runif(size.tot, min=0, max=10)
  context_data <- cbind(x1,x2,x3,x4)
  #arm reward
  arm_1 <-  as.vector(c(-1,9,-8,4))
  K1 <- crossprod(t(context_data),arm_1)
  arm_2 <-  as.vector(c(-1,2,1,0))
  K2 <- crossprod(t(context_data),arm_2)
  arm_3 <-  as.vector(c(-1,-5,1,10))
  K3 <- crossprod(t(context_data),arm_3)
  reward_data <-  cbind(K1,K2,K3)


  lapply(X, function(pol){
    pol(reward_data, context_data)
  })
}

test_normal <- function(X) {
  size.tot <- 1000
  reward_data <- gen_rewardmat(size.tot, 3)
  lapply(X, function(pol){pol(reward_data)})
}
