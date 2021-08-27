# from_distribution_policy <-
#   make_policy(
#     init_from_distribution,
#     choose_from_distribution,
#     receive_from_distribution,
#     name = "from_distribution"
#   )

# =============================

init_from_distribution <- function(k, PolArgs = list(prob=rep.int(1,k))) {
  k <- as.integer(k)
  prob <- as.numeric(PolArgs$prob)
}

choose_from_distribution <- function() {
  which <- sample(seq.int(1, k, 1), 1, prob = prob)
  list(which = which, p = prob[which])
}

receive_from_distribution <- function(arm, reward) {
}
