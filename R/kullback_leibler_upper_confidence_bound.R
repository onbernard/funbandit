
kullback_leibler_upper_confidence_bound <- structure(function(k) {
  k <- as.integer(k)

  Mu <- matrix(Inf, nrow = 1, ncol = k)
  Nu <- matrix(0, nrow = 1, ncol = k)
  t <- 1
  c <- 0
  precision <- 1e-6

  choose <- structure(function() {
    if (t <= k) {
      data.frame(which=t, stringsAsFactors = FALSE)

    }
    else{
      t2 <- sum(Nu)
      d <- (log(t2)+c*(log(log(t2))))/Nu
      upperbound <- min(1, kl_ucb_gaussian(Mu,  d))
      lowerbound <- Mu
      count_iteration <- 0
      kl_vec <- Vectorize(kl_bernoulli)
      m <- c()
      while (count_iteration < 50 && upperbound-lowerbound > precision) {
        m <- (lowerbound+upperbound)/2
        upperbound <- ifelse(kl_vec(Mu, m)>d, m, upperbound)
        lowerbound <- ifelse(kl_vec(Mu, m)<=d, m, lowerbound)
        count_iteration <- count_iteration + 1
      }
      m <- (lowerbound+upperbound)/2

      data.frame(which=which.max(m), stringsAsFactors = FALSE)
    }
  }, class="agent.choose")

  receive <- structure(function(arm, reward) {
    if (Nu[arm] == 0) {
      Mu[arm] <<- reward
    }
    else {
      Mu[arm] <<- ((Mu[arm] * Nu[arm] + reward) / (Nu[arm] + 1))
    }
    Nu[arm] <<- Nu[arm] + 1
    t <<- t+1
  }, class="agent.receive")

  structure(list(choose=choose, receive=receive), k=k, class="agent")
}, class="policy")



kl_ucb_gaussian <- function(x, d, sigma2=1){

  return ( x + sqrt(2 * sigma2 * d) )

}

kl_bernoulli <- function(p, q){

  epsilon <- 1e-16
  p = min(max(p, epsilon), 1 - epsilon)
  q = min(max(q, epsilon), 1 - epsilon)

  return(p * log(p/q) + (1 - p) * log((1 - p)/(1 - q)))

}

