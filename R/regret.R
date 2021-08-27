standard_reward <- function(h, k) {
  force(h)
  force(k)

  rewardmat <- gen_rewardmat(h, k)
  cum_regret <- 0

  reward <- function(t, choice) {
    list(arm=choice, reward=rewardmat[t, choice])
  }

  analyze <- function(t, choice, reward) {
    cum_regret <<- cum_regret + (max(rewardmat[t,]) - rewardmat[t, choice])
    list(choice=choice, reward=rewardmat[t,choice], cum_regret=cum_regret)
  }

  list(reward=reward, analyze=analyze)
}
