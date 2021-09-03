#' @include funmake.R epsilon_greedy.R exp3.R kullback_leibler_upper_confidence_bound.R thompson_sampling.R upper_confidence_bound.R
NULL

#' @incl

# cyclical <-
#   make_policy(init_cyclical, choose_cyclical, receive_cyclical, "cyclical", c())

epsilon_greedy_policy <-
  funmake(
    init_epsilon_greedy,
    choose_epsilon_greedy,
    receive_epsilon_greedy,
    c("epsilon_greedy"),
    c("epsilon")
  )


exp3_policy <-
  funmake(init_exp3, choose_exp3, receive_exp3, "exp3", c("gamma"))


# from_distribution_policy <-
#   make_policy(
#     init_from_distribution,
#     choose_from_distribution,
#     receive_from_distribution,
#     name = "from_distribution"
#   )

kullback_leibler_ucb_policy <-
  funmake(init_kl_ucb, choose_kl_ucb, receive_kl_ucb, name = "kl_ucb", c("c"))

thompson_sampling_policy <-
  funmake(
    init_thompson_sampling,
    choose_thompson_sampling,
    receive_thompson_sampling,
    "thompson_sampling",
    c("alpha", "beta")
  )

upper_confidence_bound_policy <-
  funmake(init_ucb, choose_ucb, receive_ucb, "ucb", c("alpha"))
