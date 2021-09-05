init_lints <- function(k, dim, iter = 10, v = 0.2) {
  k    <- as.integer(k)
  dim  <- as.integer(dim)
  iter <- as.integer(iter)
  v    <- as.double(v)
  # === === === === ===
  b      <- matrix(0, k, dim)
  A      <- array(diag(dim), c(dim, dim, k))
  mu_hat <- array(0, c(k, dim))
  A_inv  <- array(diag(dim), c(dim, dim, k))
  # === === === === ===
  list(
    iter   = iter,
    v      = v,
    b      = b,
    A      = A,
    mu_hat = mu_hat,
    A_inv  = A_inv
  )
}
# === ==== === ==== === ==== === ==== === ==== === ==== === ==== === ==== ===
choose_lints <- function(ctxt) {
  p <- c()
  for(j in 1:k) {
    mu_tilde <- apply(MASS::mvrnorm(iter, mu_hat[j,], v^2*A_inv[,,j]), 2, max)
    p[j] <- t(ctxt) %*% mu_tilde
  }
  list(which = which.max(p), maxts = max(p))
}
# === ==== === ==== === ==== === ==== === ==== === ==== === ==== === ==== ===
receive_lints <- function(arm, reward, ctxt) {
  A[, , arm]     <<- A[, , arm] + ctxt %*% t(ctxt)
  b[arm, ]       <<-  b[arm, ] + ctxt * reward
  A_inv[, , arm] <<- solve(A[, , arm])
  mu_hat[arm, ]  <<- A_inv[, , arm] %*% b[arm, ]
}
