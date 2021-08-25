krr_upper_confidence_bound <-
  structure(function(k,
                     dim,
                     hindsight = 100,
                     update_frequency = 100,
                     alpha = 1) {
    k <- as.integer(k)
    dim <- as.integer(dim)
    hindsight <- if(hindsight!=Inf){as.integer(hindsight)}
    update_frequency <- as.integer(update_frequency)
    alpha <- as.double(alpha)
    t <- 1

    Nu <- rep.int(0,k)

    krr_models <- vector(mode="list", length=k)
    predictors <- vector(mode="list", length=k)
    responses <- vector(mode="list", length=k)
    for(i in 1:k) {
      predictors[[i]] <- vector()
      responses[[i]] <- vector()
    }

    # =============== CHOOSE ===============
    choose <- structure(function(ctxt) {
      if (t <= k) {
        data.frame(which=t)
      }
      else {
        p <- c()
        for(i in seq_along(krr_models)) {
          p[i] <- predict(krr_models[[i]], xnew=t(ctxt))
        }
        p <- p + alpha*sqrt((2*log(t))/Nu)
        data.frame(which=which.max(p))
      }
    }, class = "contextual_agent.choose")
    # ======================================
    # =============== RECEIVE ==============
    receive <- structure(function(arm, reward, ctxt) {
      #if (t%%10 == 0) {print(t)}
      #print(t)
      Nu[arm] <<- Nu[arm] + 1
      t <<- t + 1
      predictors[[arm]] <<- c(predictors[[arm]], as.numeric(ctxt))
      responses[[arm]] <<- c(responses[[arm]], as.numeric(reward))
      if (Nu[arm]==1 || Nu[arm] %% update_frequency == 0) {
        pred <- matrix(predictors[[arm]], ncol=dim)
        res  <- matrix(responses[[arm]], ncol=1)
        #print(max(1,Nu[arm]-hindsight):Nu[arm])
        pred <- pred[max(1,Nu[arm]-hindsight):Nu[arm],, drop=F]
        res  <- res[max(1, Nu[arm]-hindsight):Nu[arm],, drop=F]
        message(paste(c("update", arm, Nu[arm]), collapse=" "))
        krr_models[[arm]] <<- listdtr::krr(pred, res)
      }
    }, class = "contextual_agent.receive")
    # ======================================

    structure(
      list(choose = choose, receive = receive),
      k = k,
      name = "kernelucb",
      class = "contextual_agent"
    )
  }, class = c("contextual_policy", "policy"))

