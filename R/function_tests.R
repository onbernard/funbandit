#
#
# test_appl <- function(which_apply, which_pol, upto) {
#   force(which_apply)
#   force(which_pol)
#   force(upto)
#   data <- gen_data(upto)
#   function(n) {
#     which_apply(which_pol, data[seq_len(n),])
#   }
# }
#
# apply_policy_rbind <- function(policy, rewards, PolicyArgs=NULL) {
#   rewards <- as.matrix(rewards)
#   pol <- c()
#   if (is.null(PolicyArgs)) {
#     pol <- policy(k=ncol(rewards))
#   }
#   else {
#     pol <- policy(k=ncol(rewards), unlist(PolicyArgs))
#   }
#
#   h <- nrow(rewards)
#   k <- ncol(rewards)
#   mkRow <- fancy_results(pol, rewards)
#
#   d <- mkRow(1)
#   nCol <- ncol(d)
#   for(i in 2:h) {
#     d <- rbind(d, mkRow(i))
#   }
#   d
# }
#
# apply_policy_nohack <- function(policy, rewards, PolicyArgs=NULL) {
#   rewards <- as.matrix(rewards)
#   pol <- c()
#   if (is.null(PolicyArgs)) {
#     pol <- policy(k=ncol(rewards))
#   }
#   else {
#     pol <- policy(k=ncol(rewards), unlist(PolicyArgs))
#   }
#
#   h <- nrow(rewards)
#   k <- ncol(rewards)
#   mkRow <- fancy_results(pol, rewards)
#
#   d <- mkRow(1)
#   d <- d[rep.int(1,h),]
#   nCol <- ncol(d)
#   if (h>1) {
#     d <- d[rep.int(1,h),]
#     for (i in seq.int(2,h,1)) {
#       ri <- mkRow(i)
#       for (j in seq_len(nCol)) {
#         d[[j]][i] <- ri[[j]]
#       }
#     }
#   }
#   d
# }
#
# apply_policy <- function(policy, rewards, PolicyArgs=NULL) {
#   rewards <- as.matrix(rewards)
#   pol <- c()
#   name <- deparse1(substitute(policy))
#   if (is.null(PolicyArgs)) {
#     pol <- policy(k=ncol(rewards))
#   }
#   else {
#     pol <- policy(k=ncol(rewards), unlist(PolicyArgs))
#     name <- paste(name, deparse1(substitute(PolicyArgs)))
#   }
#
#   h <- nrow(rewards)
#   k <- ncol(rewards)
#   mkRow <- mkRound(pol, rewards)
#
#   d <- mkRow(1)
#   d <- d[rep.int(1,h),]
#   nCol <- ncol(d)
#   if (h>1) {
#     d <- d[rep.int(1,h),]
#     d <- as.list(d)
#     for (i in seq.int(2,h,1)) {
#       ri <- mkRow(i)
#       for (j in seq_len(nCol)) {
#         d[[j]][i] <- ri[[j]]
#       }
#     }
#   }
#   df <- data.frame(d, stringsAsFactors = FALSE)
#   df$pol <- name
#   df$pol <- factor(df$pol)
#   df
# }
#
# # TODO : test apply as an aggregate of reduce


