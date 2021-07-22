from_distribution <- function(k, prob) {
  force(k)
  force(prob)

  choose <- structure(function() {
    which <- sample(seq.int(1,k,1), 1, prob = prob)
    tibble(which=which, p=prob[which])
  }, class="agent.choose")

  receive <- structure(function(arm, reward) {}, class="agent.receive")

  structure(list(choose=choose, receive=receive), class="agent")
}
