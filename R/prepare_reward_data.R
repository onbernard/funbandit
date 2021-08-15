prepare_reward_data <- function(reward_data) {
  rewardmat <- c()
  if(is.vector(reward_data, mode="numeric")){
    rewardmat <- matrix(reward_data, nrow=1)
  }
  else {rewardmat <- as.matrix(reward_data)}
  rewardmat
}
