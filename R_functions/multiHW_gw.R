MultiHW <- function(TS, K, L, H) {
  previsoes <- matrix(NA, nrow = H, ncol = ncol(TS))
  
  for (i in 1:ncol(TS)) {
    Test <- K
    S <- round(K / 1)
    Runs <- 8
    holdout_res <- holdout_data(TS, K, S, Runs, L, i)
    H <- holdout_res$H
    dtr <- holdout_res$dtr
    
    # Modelo Holt-Winters
    modelo_hw <- HoltWinters(dtr)
    Pred_hw <- forecast(modelo_hw, h = length(H$ts))$mean
    metrics <- calc_metrics(TS[H$ts, i], Pred_hw)
    
    
    # cat(sprintf("\nMedianas de métricas para o departamento %d do método Holtwinters:\n", i))
    # cat(sprintf("Mediana NMAE: %f\n", metrics$nmae))
    # cat(sprintf("Mediana MAE: %f\n", metrics$mae))
    # cat(sprintf("Mediana RMSE: %f\n", metrics$rmse))
    # cat(sprintf("Mediana R2: %f\n", metrics$r2))
    
    Pred_future_last <- forecast(modelo_hw, h = 4)$mean
    previsoes[, i] <- Pred_future_last
  }
  
  results <- list(previsoes, metrics)
  
  return(results)
}