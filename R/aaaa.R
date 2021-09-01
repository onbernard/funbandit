rewards_from_matrix <- function(reward_data) {
  reward_data <- as.matrix(reward_data)

  function(t, choice) {
    r <- reward_data[t, choice$which]
    list(arm=choice$which, reward=r)
  }
}

info_regret <- function(reward_data) {
  reward_data <- as.matrix(reward_data)
  cumulative_regret <- 0

  function(t, choice, reward) {
    arm <- reward$arm
    r <- reward$reward
    cumulative_regret <<- cumulative_regret + max(reward_data[t,]) - reward_data[t, arm]
    c(choice, reward, list(cum_regret=cumulative_regret))
  }
}
