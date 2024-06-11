# Instalar pacotes se necessário
if (!requireNamespace("forecast", quietly = TRUE)) {
  install.packages("forecast")
}
if (!requireNamespace("rminer", quietly = TRUE)) {
  install.packages("rminer")
}

# Carregar bibliotecas
library(forecast)
library(rminer)

# Função para criar modelos ARIMAX
autoARIMAX <- function(mtr, frequency = 1) {
  models <- ncol(mtr)
  res <- vector("list", length = models)
  res2 <- vector("list", length = models)
  for(i in 1:models) {
    nr <- nrow(mtr)  # Definir o número de linhas
    if(frequency > 1) s_tr <- ts(mtr[1:nr, i], frequency = frequency) else s_tr <- ts(mtr[1:nr, i])
    xreg <- mtr[1:nr, -i]  # Dados de regressão para o modelo ARIMAX
    if (is.matrix(xreg)) {
      res[[i]] <- auto.arima(s_tr, xreg = xreg)
    } else {
      res[[i]] <- auto.arima(s_tr, xreg = as.matrix(xreg))
    }
    res2[[i]] <- auto.arima(s_tr)
  }
  return(list(arimax = res, arima = res2))
}  

# Função para fazer previsões ARIMAX
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

# Ler série temporal do Walmart
cat("Lendo série temporal do Walmart:\n")
dados <- read.csv("walmart.csv")
# Selecionar colunas dos departamentos
TS <- dados[, c("WSdep1", "WSdep2", "WSdep3", "WSdep4")]
K <- 4 # Período da série temporal (mensal!)

L <- nrow(TS)
NTS <- K # Número de previsões
H <- NTS # de 1 a H previsões à frente

# Objeto de série temporal mensal, frequência=K 
LTR <- L - H
TR <- ts(TS[1:LTR, ], frequency = K)

# Previsões alvo
Y <- TS[(LTR + 1):L, ]

# Vetor para armazenar as previsões
previsoes <- matrix(NA, nrow = H, ncol = ncol(TS)) 

# Criar modelo ARIMAX para cada departamento
modelos <- autoARIMAX(TS, frequency = 12)  # Assumindo frequência mensal

# Método de previsão ARIMAX com Growing Window para cada departamento
for (i in 1:ncol(TS)) {
  # Growing window para encontrar a melhor iteração
  Test <- K  # Número de previsões igual a K
  S <- round(K / 1)  # Tamanho do passo para a janela deslizante
  Runs <- 8  # Número de iterações da janela deslizante
  W <- (L - Test) - (Runs - 1) * S  # Tamanho inicial da janela de treinamento
  
  # Vetores para armazenar as métricas para cada iteração
  nmae <- numeric(Runs)
  mae <- numeric(Runs)
  rmse <- numeric(Runs)
  r2 <- numeric(Runs)
  
  # Growing window
  for (b in 1:Runs) {
    H <- holdout(TS[, i], ratio = Test, mode = "incremental", iter = b, window = W, increment = S)
    trinit <- H$tr[1]
    dtr <- ts(TS[H$tr, i], frequency = K)  # Criar objeto ts
    M <- suppressWarnings(auto.arima(dtr))  # Criar modelo ARIMA
    Pred_arimax <- forecastARIMAX(modelos, h = length(H$ts))[[i]]  # Previsões ARIMAX
    nmae[b] <- mmetric(y = TS[H$ts, i], x = Pred_arimax, metric = "NMAE")
    mae[b] <- mmetric(y = TS[H$ts, i], x = Pred_arimax, metric = "MAE")
    rmse[b] <- mmetric(y = TS[H$ts, i], x = Pred_arimax, metric = "RMSE")
    r2[b] <- mmetric(y = TS[H$ts, i], x = Pred_arimax, metric = "R2")
  }
  
  # Calcular a mediana das métricas
  mediana_nmae <- median(nmae)
  mediana_mae <- median(mae)
  mediana_rmse <- median(rmse)
  mediana_r2 <- median(r2)
  
  # Usar a última iteração para fazer as previsões
  b <- Runs
  
  H_last <- holdout(TS[, i], ratio = Test, mode = "incremental", iter = b, window = W, increment = S)
  Pred_future_last <- forecastARIMAX(modelos, h = 4)[[i]]  # Fazer previsões para os próximos 4 períodos
  previsoes[, i] <- Pred_future_last  # Armazenar previsões na matriz
  
  # Imprimir as métricas medianas
  cat(sprintf("\nMétricas medianas para o departamento %d:\n", i))
  cat(sprintf("Mediana NMAE: %f\n", mediana_nmae))
  cat(sprintf("Mediana MAE: %f\n", mediana_mae))
  cat(sprintf("Mediana RMSE: %f\n", mediana_rmse))
  cat(sprintf("Mediana R2: %f\n", mediana_r2))
}

# Imprimir previsões
# cat("\nPrevisões ARIMAX :\n")
# print(previsoes)