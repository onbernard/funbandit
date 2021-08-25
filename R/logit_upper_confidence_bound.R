logit_upper_confidence_bound <- structure(function(k, dim, alpha = 1) {
  k <- as.integer(k)
  alpha <- as.double(alpha)
  dim <- as.integer(dim)

  b <- matrix(0, k, dim)
  A <- array(diag(dim), c(dim, dim, k))

  th_hat <- array(0, c(k, dim))
  A_inv <- array(diag(dim), c(dim, dim, k))

  choose <- structure(function(ctxt) {
    a_upper_ci <- apply(A_inv, 3, function(x) {
      t(ctxt) %*% x %*% ctxt
    }) %>% sqrt
    a_mean <- apply(th_hat, 1, function(x) {
      x %*% ctxt
    })
    p_mean <- 1/(1+exp(-a_mean))
    p <- alpha * a_upper_ci + p_mean
    data.frame(which = which.max(p), maxucb = max(p))
  }, class="contextual_agent.choose")

  receive <- structure(function(arm, reward, ctxt) {
    A[, , arm] <<- A[, , arm] + ctxt %*% t(ctxt)
    b[arm, ] <<-  b[arm, ] + ctxt * reward
    A_inv[, , arm] <<- solve(A[, , arm])
    th_hat[arm, ] <<- A_inv[, , arm] %*% b[arm, ]
  }, class="contextual_agent.receive")

  structure(list(choose=choose, receive=receive), k=k, name="logitucb", class="contextual_agent")
}, class=c("contextual_policy", "policy"))


# for (j in 1:K) {
#   A_inv      = solve(A[,,j])
#   th_hat[j,] = A_inv %*% b[j,]
#   ta         = t(x_i) %*% A_inv %*%  x_i
#   a_upper_ci = alpha * sqrt(ta)             # upper part of variance interval
#   a_mean     = th_hat[j,] %*% x_i              # current estimate of mean
#   proba_mean =  1/(1+exp(-a_mean))  # inverse logit transform of linear predictor
#   p[j]       = proba_mean + a_upper_ci         # top CI
# }
