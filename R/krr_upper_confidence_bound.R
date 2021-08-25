krr_upper_confidence_bound <-
  structure(function(k,
                     dim,
                     hindsight = Inf,
                     alpha = 1) {
    k <- as.integer(k)
    dim <- as.integer(dim)
    hindsight <- if(hindsight!=Inf){as.integer(hindsight)}
    alpha <- as.double(alpha)
    t <- 1

    Nu <- rep.int(0,k)

    krr_models <- vector(mode="list", length=k)
    predictors <- vector(mode="list", length=k)
    responses <- vector(mode="list", length=k)
    for(i in 1:k) {
      predictors[[i]] <- matrix(0, nrow=0, ncol=dim)
      responses[[i]] <- matrix(0, nrow=0, ncol=dim)
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
      if (t%%10 == 0) {print(t)}

      predictors[[arm]] <<- rbind(predictors[[arm]], ctxt)
      responses[[arm]] <<- rbind(responses[[arm]], reward)
      krr_models[[arm]] <<- listdtr::krr(predictors[[arm]], responses[[arm]])
      Nu[arm] <<- Nu[arm] + 1
      t <<- t + 1
    }, class = "contextual_agent.receive")
    # ======================================

    structure(
      list(choose = choose, receive = receive),
      k = k,
      name = "kernelucb",
      class = "contextual_agent"
    )
  }, class = c("contextual_policy", "policy"))

