linear_upper_confidence_bound <- function(k, dim, alpha = 1) {
  force(k)
  force(alpha)
  force(dim)

  b <- matrix(0, k, dim)
  A <- array(diag(dim), c(dim, dim, k))

  th_hat <- array(0, c(k, dim))
  A_inv <- array(diag(dim), c(dim, dim, k))

  choose <- function(ctxt) {
    a_upper_ci <- apply(A_inv, 3, function(x) {
      t(ctxt) %*% x %*% ctxt
    }) %>% sqrt
    a_mean <- apply(th_hat, 1, function(x) {
      x %*% ctxt
    })
    p <- alpha * a_upper_ci + a_mean
    tibble(which = which.max(p), maxucb = max(p))
  }

  receive <- function(arm, reward, ctxt) {
    A[, , arm] <<- A[, , arm] + ctxt %*% t(ctxt)
    b[arm, ] <<-  b[arm, ] + ctxt * reward
    A_inv[, , arm] <<- solve(A[, , arm])
    th_hat[arm, ] <<- A_inv[, , arm] %*% b[arm, ]
  }

  list(choose=choose, receive=receive)
}
