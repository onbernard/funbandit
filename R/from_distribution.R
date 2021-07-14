from_distribution <- function(k, prob) {
  force(k)
  force(prob)

  choose <- function() {
    which <- sample(seq.int(1,k,1), 1, prob = prob)
    list(which=which, p=prob[which])
  }

  receive <- function(arm, reward) {}

  list(choose=choose, receive=receive)
}
