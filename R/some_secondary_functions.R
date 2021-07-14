round <- function(policy) {
  function(rewards) {
    whatnext <- policy$choose()
    choice <- whatnext$which
    reason <- whatnext[-1]
    policy$receive(choice, rewards[choice])
    whatnext
  }
}

# TODO : make process take a policy constructor not a function
process <- function(policy) {
  polround <- round(policy)
  function(reward_matrix) {
    h <- nrow(reward_matrix)
    result <- tibble(choice=rep(0,h), reason=rep(0,h))
    for (t in 1:h) {
      result[t,] <- polround(reward_matrix[t,])
    }
    result
  }
}
