# Instalar pacotes se necessário
install_packages <- function(packages) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
  }
}

# Pacotes necessários
packages <- c("forecast", "rminer", "vars")
install_packages(packages)

# Carregar bibliotecas
library(forecast)
library(rminer)
library(vars)

# Ler dados
cat("Lendo série temporal do Walmart:\n")
dados <- read.csv("walmart.csv")
TS <- dados[, c("WSdep1", "WSdep2", "WSdep3", "WSdep4")]
K <- 4 # Período da série temporal (mensal!)
L <- nrow(TS)
NTS <- K # Número de previsões
H <- NTS # de 1 a H previsões à frente

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

# Chamar métodos ARIMAX, VAR e Holt-Winters
source("arimax_method.R")
source("var_method.R")
source("holtwinters_method.R")

# Previsões e métricas para cada departamento
previsoes_arimax <- run_arimax(TS, K, L, H)
cat("\nPrevisões ARIMAX:\n")
print(previsoes_arimax)

previsoes_var <- run_var(TS, K, L, H)
cat("\nPrevisões VAR:\n")
print(previsoes_var)


previsoes_holtwinters <- run_holtwinters(TS, K, L, H)
cat("\nPrevisões Holt-Winters:\n")
print(previsoes_holtwinters)