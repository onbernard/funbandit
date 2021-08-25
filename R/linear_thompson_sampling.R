linear_thompson_sampling <- structure(function(k, dim, iter=10, v=0.2) {
  force(k)
  force(dim)
  force(iter)

  b <- matrix(0, k, dim)
  A <- array(diag(dim), c(dim, dim, k))

  mu_hat <- array(0, c(k, dim))
  A_inv <- array(diag(dim), c(dim, dim, k))


  choose <- structure(function(ctxt) {

    p <- c()
    for(j in 1:k) {
      mu_tilde <- apply(MASS::mvrnorm(iter, mu_hat[j,], v^2*A_inv[,,j]), 2, max)
      p[j] <- t(ctxt) %*% mu_tilde
    }

    data.frame(which = which.max(p), maxts = max(p))
  }, class="contextual_agent.choose")

  receive <- structure(function(arm, reward, ctxt) {
    A[, , arm] <<- A[, , arm] + ctxt %*% t(ctxt)
    b[arm, ] <<-  b[arm, ] + ctxt * reward
    A_inv[, , arm] <<- solve(A[, , arm])
    mu_hat[arm, ] <<- A_inv[, , arm] %*% b[arm, ]
  }, class="contextual_agent.receive")

  structure(list(choose=choose, receive=receive), k=k, class="contextual_agent")
}, class=c("contextual_policy", "policy"))
