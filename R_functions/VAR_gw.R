VAR_pred <- function(TS, K, L, H) {
  autoVAR <- function(mtr, LAGMAX = 10) {  
    vselect <- VARselect(mtr, lag.max = LAGMAX)
    omin <- as.numeric(vselect$selection[3]) 
    omax <- as.numeric(vselect$selection[1]) 
    stop <- FALSE
    pvalueref <- 0.10
    o <- omin
    while(!stop) {
      mvar <- VAR(mtr, p = o)
      st <- serial.test(mvar, lags.pt = LAGMAX, type = "PT.asymptotic")
      pvalue <- as.numeric(st$serial$p.value)
      if(pvalue > pvalueref) stop <- TRUE
      else if((o + 1) == omax) {stop <- TRUE; o <- o + 1} 
      else o <- o + 1
    }
    mvar <- VAR(mtr, p = o)
    return(mvar)
  }
  
  forecastVAR <- function(model, h) {
    models <- length(model$varresult)
    res <- vector("list", length = models)
    F1 <- predict(model, n.ahead = h)$fcst
    for(i in 1:models) res[[i]] <- as.numeric(F1[[i]][, "fcst"])
    return(res)
  }
  
  TR <- ts(TS[1:(L-H), ], frequency = K)
  modelos_VAR <- autoVAR(TR, LAGMAX = 12)
  previsoes <- matrix(NA, nrow = 4, ncol = ncol(TS))
  
  for (i in 1:ncol(TS)) {
    Test <- K
    S <- round(K / 1)
    Runs <- 8
    holdout_res <- holdout_data(TS, K, S, Runs, L, i)
    H <- holdout_res$H
    dtr <- holdout_res$dtr
    
    Pred_var <- forecastVAR(modelos_VAR, h = length(H$ts))[[i]]
    metrics <- calc_metrics(TS[H$ts, i], Pred_var)
    
    # cat(sprintf("\nMedianas de métricas para o departamento %d do método VAR:\n", i))
    # cat(sprintf("Mediana NMAE: %f\n", metrics$nmae))
    # cat(sprintf("Mediana MAE: %f\n", metrics$mae))
    # cat(sprintf("Mediana RMSE: %f\n", metrics$rmse))
    # cat(sprintf("Mediana R2: %f\n", metrics$r2))
    
    Pred_future_last <- forecastVAR(modelos_VAR, h = 4)[[i]]
    previsoes[, i] <- Pred_future_last
  }
  
  results <- list(previsoes, metrics)
  
  return(results)
}