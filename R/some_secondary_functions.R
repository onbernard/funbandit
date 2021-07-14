round <- function(policy) {
  function(rewards) {
    choice <- policy$whatnext()$which
    policy$nowwhat(choice, rewards[choice])
  }
}
