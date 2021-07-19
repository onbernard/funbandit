test_linucb <- function() {
  size.tot = 1000
  set.seed(4649)                          # this makes the example exactly reproducible
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

  pol <- linear_upper_confidence_bound(3, 4)

  choices <- rep(0,1000)
  proba <- rep(0, 1000)
  for(i in 1:1000) {
    whatnext <- pol$choose(dt[i,])
    choices[i] <- whatnext$which
    proba[i] <- whatnext$maxucb
    pol$receive(choices[i], visitor_reward[i,choices[i]], dt[i,])
  }
  choices
}

gen_data <- function(n) {
  outp <- matrix(NA, ncol=5, nrow=n)
  for(a in 1:5) {
    outp[,a] <- rbinom(n, 1, runif(1))
  }
  outp
}

test_compx <- function(n) {
  system.time({invisible(apply_policy(upper_confidence_bound, gen_data(n)))})["elapsed"]
}
