

# =============================

init_from_distribution <- function(k, prob=rep.int(1,k)) {
  k <- as.integer(k)
  prob <- as.numeric(prob)
}

choose_from_distribution <- function() {
  which <- sample(seq.int(1, k, 1), 1, prob = prob)
  list(which = which, p = prob[which])
}

receive_from_distribution <- function(arm, reward) {
}


