# Load necessary libraries
library(forecast)
library(rminer)
library(vars)

source("R_functions//arimax_gw.R")
source("R_functions//multiHW_gw.R")
source("R_functions//VAR_gw.R")

dados <- read.csv("Data//walmart.csv",sep=",",header=TRUE)
TS <- dados[, c("WSdep1", "WSdep2", "WSdep3", "WSdep4")]
K <- 4 # Período da série temporal (mensal!)
L <- nrow(TS)
NTS <- K # Número de previsões
H <- NTS # de 1 a H previsões à frente

Runs = 8

# Função comum para dividir dados de treino e teste
holdout_data <- function(data, K, S, Runs, L, i) {
  W <- (L - K) - (Runs - 1) * S
  H <- holdout(data[, i], ratio = K, mode = "incremental", iter = Runs, window = W, increment = S)
  dtr <- ts(data[H$tr, i], frequency = K)
  list(H = H, dtr = dtr)
}

# Função para calcular métricas
calc_metrics <- function(actual, predicted) {
  nmae <- mmetric(y = actual, x = predicted, metric = "NMAE")
  mae <- mmetric(y = actual, x = predicted, metric = "MAE")
  rmse <- mmetric(y = actual, x = predicted, metric = "RMSE")
  r2 <- mmetric(y = actual, x = predicted, metric = "R2")
  list(nmae = nmae, mae = mae, rmse = rmse, r2 = r2)
}

MultivariateModel = function(multi_model, month){
  multi_model <- as.integer(multi_model)
  
  print(multi_model)
  month <- as.integer(month)
  
  for(b in 1:Runs)  {
    
    switch(multi_model,
           `1` = {
             method_name <- "ARIMAX"
             print(method_name)
             results <- Arimax(TS, K, L, H)
           },
           `2` = {
             method_name <- "MultiHW"
             print(method_name)
             results <- MultiHW(TS, K, L, H)
           },
           `3` = {
             method_name <- "VAR"
             print(method_name)
             results <- VAR_pred(TS, K, L, H)
           })

  }
  return(results)
}

#MultivariateModel(1,13)