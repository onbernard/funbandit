# TODO
linear_contextual <- function(k, dim, indices, alpha = 1) {
  k <- as.integer(k)
  alpha <- as.double(alpha)
  dim <- as.integer(dim)

  b <- matrix(0, k, dim)
  A <- array(diag(dim), c(dim, dim, k))

  th_hat <- array(0, c(k, dim))
  A_inv <- array(diag(dim), c(dim, dim, k))

  choose <- structure(function(ctxt) {
    p <- indices(A, A_inv, b, th_hat, ctxt)
    data.frame(which = which.max(p), maxucb = max(p))
  }, class="contextual_agent.choose")

  receive <- structure(function(arm, reward, ctxt) {
    A[, , arm] <<- A[, , arm] + ctxt %*% t(ctxt)
    b[arm, ] <<-  b[arm, ] + ctxt * reward
    A_inv[, , arm] <<- solve(A[, , arm])
    th_hat[arm, ] <<- A_inv[, , arm] %*% b[arm, ]
  }, class="contextual_agent.receive")

  structure(list(choose=choose, receive=receive), k=k, class="contextual_agent")
}

