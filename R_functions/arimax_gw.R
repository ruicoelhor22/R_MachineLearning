Arimax <- function(TS, K, L, H) {
  autoARIMAX <- function(mtr, frequency = 1) {
    models <- ncol(mtr)
    res <- vector("list", length = models)
    res2 <- vector("list", length = models)
    for(i in 1:models) {
      nr <- nrow(mtr)
      if(frequency > 1) s_tr <- ts(mtr[1:nr, i], frequency = frequency) else s_tr <- ts(mtr[1:nr, i])
      xreg <- mtr[1:nr, -i]
      if (is.matrix(xreg)) {
        res[[i]] <- auto.arima(s_tr, xreg = xreg)
      } else {
        res[[i]] <- auto.arima(s_tr, xreg = as.matrix(xreg))
      }
      res2[[i]] <- auto.arima(s_tr)
    }
    return(list(arimax = res, arima = res2))
  }  
  
  forecastARIMAX <- function(model, h) {
    models <- length(model$arimax)
    res <- vector("list", length = models)
    xreg <- matrix(ncol = models, nrow = h)
    for(i in 1:models) xreg[, i] <- forecast(model$arima[[i]], h = h)$mean
    for(i in 1:models) {
      if(length(xreg[1, -i]) == 1) {xaux <- matrix(ncol = 1, nrow = h); xaux[, 1] <- xreg[, -i]}  
      else xaux <- xreg[, -i]
      F1 <- suppressWarnings(forecast(model$arimax[[i]], h = h, xreg = xaux))
      res[[i]] <- as.numeric(F1$mean)
    }
    return(res)
  }
  
  modelos <- autoARIMAX(TS, frequency = 12)
  previsoes <- matrix(NA, nrow = H, ncol = ncol(TS))
  
  for (i in 1:ncol(TS)) {
    Test <- K
    S <- round(K / 1)
    Runs <- 8
    holdout_res <- holdout_data(TS, K, S, Runs, L, i)
    H <- holdout_res$H
    dtr <- holdout_res$dtr
    
    Pred_arimax <- forecastARIMAX(modelos, h = length(H$ts))[[i]]
    metrics <- calc_metrics(TS[H$ts, i], Pred_arimax)
    
    # cat(sprintf("\nMedianas de métricas para o departamento %d do método ARIMAX:\n", i))
    # cat(sprintf("Mediana NMAE: %f\n", metrics$nmae))
    # cat(sprintf("Mediana MAE: %f\n", metrics$mae))
    # cat(sprintf("Mediana RMSE: %f\n", metrics$rmse))
    # cat(sprintf("Mediana R2: %f\n", metrics$r2))
    
    Pred_future_last <- forecastARIMAX(modelos, h = 4)[[i]]
    previsoes[, i] <- Pred_future_last
  }
  
  results <- list(previsoes, metrics)
  
  return(results)
}