#' @export
choose <- function(agent, ...) {
  UseMethod("choose")
}

#' @export
choose.agent <- function(agent, ...) {
  agent$choose(...)
}

#' @export
receive <- function(agent, arm, reward, ...) {
  UseMethod("receive")
}

#' @export
receive.agent <- function(agent, arm, reward, ...) {
  agent$receive(arm, reward, ...)
}

#' @export
feed <- function(agent, reward_data, ...) {
  UseMethod("feed")
}

#' @export
feed.agent <- function(agent, reward_data) {
  reward_data <- as.numeric(reward_data)

  whatdo <- agent$choose()
  agent$receive(whatdo$which, reward_data[whatdo$which])

  whatdo
}

# TODO : check whether as.numeric changes behaviour of t()
#' @export
feed.contextual_agent <- function(agent, reward_data, context) {
  reward_data <- as.numeric(reward_data)
  context <- as.numeric(context)

  whatdo <- agent$choose(context)
  agent$receive(whatdo$which, reward_data[whatdo$which], context)

  whatdo
}
