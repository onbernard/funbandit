choose <- function(agent, ...) {
  UseMethod("choose")
}

choose.agent <- function(agent, ...) {
  agent$choose(...)
}

receive <- function(agent, arm, reward, ...) {
  UseMethod("receive")
}

receive.agent <- function(agent, arm, reward, ...) {
  agent$receive(arm, reward, ...)
}


feed <- function(agent, reward_data, ...) {
  UseMethod("feed")
}

feed.agent <- function(agent, reward_data) {
  reward_data <- as.numeric(reward_data)

  whatdo <- agent$choose()
  agent$receive(whatdo$which, reward_data[whatdo$which])

  whatdo
}

# TODO : check whether as.numeric changes behaviour of t()
feed.contextual_agent <- function(agent, reward_data, context) {
  reward_data <- as.numeric(reward_data)
  context <- as.numeric(context)

  whatdo <- agent$choose(context)
  agent$receive(whatdo$which, reward_data[whatdo$which], context)

  whatdo
}
