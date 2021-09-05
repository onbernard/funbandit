# === ==== === ==== === ==== === ==== === ==== === ==== === ==== === ==== ===
init_logit_ucb <- function(k, dim, alpha = 1) {
  k     <- as.integer(k)
  alpha <- as.double(alpha)
  dim   <- as.integer(dim)
  # === === === === ===
  b      <- matrix(0, k, dim)
  A      <- array(diag(dim), c(dim, dim, k))
  th_hat <- array(0, c(k, dim))
  A_inv  <- array(diag(dim), c(dim, dim, k))
  # === === === === ===
  list(
    alpha  = alpha,
    b      = b,
    A      = A,
    th_hat = th_hat,
    A_inv  = A_inv
  )
}
# === ==== === ==== === ==== === ==== === ==== === ==== === ==== === ==== ===
choose_logit_ucb <- function(ctxt) {
  a_upper_ci <- apply(A_inv, 3, function(x) {
    t(ctxt) %*% x %*% ctxt
  }) %>% sqrt
  a_mean <- apply(th_hat, 1, function(x) {
    x %*% ctxt
  })
  p_mean <- 1/(1+exp(-a_mean))
  p <- alpha * a_upper_ci + p_mean
  list(which = which.max(p), maxucb = max(p))
}
# === ==== === ==== === ==== === ==== === ==== === ==== === ==== === ==== ===
receive_logit_ucb <- function(arm, reward, ctxt) {
  A[, , arm]     <<- A[, , arm] + ctxt %*% t(ctxt)
  b[arm, ]       <<-  b[arm, ] + ctxt * reward
  A_inv[, , arm] <<- solve(A[, , arm])
  th_hat[arm, ]  <<- A_inv[, , arm] %*% b[arm, ]
}
