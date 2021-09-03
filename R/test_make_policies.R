# test_choose <- function() {
#   if (t <= k) {
#     list(which = t, why="explore")
#   }
#   else {
#     whatdo <- exploit_or_not(epsilon)
#     which <- switch(
#       whatdo,
#       "exploit" = which.max(Mu),
#       "explore" = sample(1:k, size = 1, replace = TRUE)
#     )
#     list(which=which, why=whatdo)
#   }
# }
#
# test_receive <- function(arm, reward) {
#   if (Nu[arm] == 0) {
#     Mu[arm] <<- reward
#   }
#   else {
#     Mu[arm] <<- ((Mu[arm] * Nu[arm] + reward) / (Nu[arm] + 1))
#   }
#   Nu[arm] <<- Nu[arm] + 1
#   t <<- t + 1
# }
#
# test_init <- function(k, PolArgs=list(epsilon=0.25)) {
#
#   epsilon <- as.double(PolArgs$epsilon)
#   k <- as.integer(k)
#
#   Mu <- matrix(Inf, nrow = 1, ncol = k)
#   Nu <- matrix(0, nrow = 1, ncol = k)
#   t <- 1
#   list(epsilon=epsilon, k=k, Mu=Mu, Nu=Nu, t=t)
# }
#
# test_pol <- make_policy(test_init, test_choose, test_receive, "testeps")
