init_krr_ucb <- function(k,
                         dim,
                         hindsight = 100,
                         update_frequency = 100,
                         alpha = 1) {
  k         <- as.integer(k)
  dim       <- as.integer(dim)
  hindsight <- if(hindsight!=Inf){as.integer(hindsight)}
  update_frequency <- as.integer(update_frequency)
  alpha     <- as.double(alpha)
  # === === === === ===
  Nu         <- rep.int(0,k)
  t          <- 1
  krr_models <- vector(mode="list", length=k)
  predictors <- vector(mode="list", length=k)
  responses  <- vector(mode="list", length=k)
  for(i in 1:k) {
    predictors[[i]] <- vector()
    responses[[i]]  <- vector()
  }
  # === === === === ===
  list(
    hindsight  = hindsight,
    update_frequency = update_frequency,
    alpha      = alpha,
    t          = t,
    Nu         = Nu,
    krr_models = krr_models,
    predictors = predictors,
    responses  = responses
  )
}
# === ==== === ==== === ==== === ==== === ==== === ==== === ==== === ==== ===
choose_krr_ucb <- function(ctxt) {
  if (t <= k) {
    list(which=t)
  }
  else {
    p <- c()
    for(i in seq_along(krr_models)) {
      p[i] <- predict(krr_models[[i]], xnew=t(ctxt))
    }
    p <- p + alpha*sqrt((2*log(t))/Nu)
    list(which=which.max(p))
  }
}
# === ==== === ==== === ==== === ==== === ==== === ==== === ==== === ==== ===
receive_krr_ucb <- function(arm, reward, ctxt) {
  Nu[arm] <<- Nu[arm] + 1
  t       <<- t + 1
  predictors[[arm]] <<- c(predictors[[arm]], as.numeric(ctxt))
  responses[[arm]]  <<- c(responses[[arm]], as.numeric(reward))
  if (Nu[arm]==1 || Nu[arm] %% update_frequency == 0) {
    pred <- matrix(predictors[[arm]], ncol=dim)
    res  <- matrix(responses[[arm]], ncol=1)
    pred <- pred[max(1,Nu[arm]-hindsight):Nu[arm],, drop=F]
    res  <- res[max(1, Nu[arm]-hindsight):Nu[arm],, drop=F]
    # === === === === ===
    message(paste(c("update", arm, Nu[arm]), collapse=" "))
    krr_models[[arm]] <<- listdtr::krr(pred, res)
  }
}
