library(devtools)
library(tidyverse)
library(sloop)
library(ggpubr)
library(microbenchmark)
library(ggplot2)

gen_rewardmat <- function(h, k, prob=runif(k)) {
  mat <- matrix(NA, ncol=k, nrow=h)
  for(a in 1:k) {
    mat[,a] <- rbinom(h, 1, prob[a])
  }
  mat
}

test_contextual <- function(h=1000) {
  x1 <- runif(h, min=0, max=10)
  x2 <- runif(h, min=0, max=10)
  x3 <- runif(h, min=0, max=10)
  x4 <- runif(h, min=0, max=10)
  context_data <- cbind(x1,x2,x3,x4)
  #arm reward
  arm_1 <-  as.vector(c(-1,9,-8,4))
  K1 <- crossprod(t(context_data),arm_1)
  arm_2 <-  as.vector(c(-1,2,1,0))
  K2 <- crossprod(t(context_data),arm_2)
  arm_3 <-  as.vector(c(-1,-5,1,10))
  K3 <- crossprod(t(context_data),arm_3)
  reward_data <-  cbind(K1,K2,K3)

  X <- list(
            kernel_upper_confidence_bound = aggregate_agent(krr_upper_confidence_bound),
            linear_upper_confidence_bound = aggregate_agent(linear_upper_confidence_bound),
            linear_thompson_sampling = aggregate_agent(linear_thompson_sampling)
        )

  r <- lapply(X, function(pol){
    pol(reward_data, context_data)
  })
  compare_results(r)
}

test_normal <- function(h=200, k=3) {
  reward_data <- gen_rewardmat(h, k)
  X <-
    list(
      upper_confidence_bound = aggregate_agent(upper_confidence_bound_policy),
      thompson_sampling = aggregate_agent(thompson_sampling_policy),
      exp3 = aggregate_agent(exp3_policy),
      epsilon_greedy = aggregate_agent(epsilon_greedy_policy),
      test_constructor = aggregate_agent(test_pol),
      kullback_leibler_upper_confidence_bound = aggregate_agent(kullback_leibler_ucb_policy)
    )
  r  <- lapply(X, function(pol){pol(reward_data)})
  compare_results(r)
}
