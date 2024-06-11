library(forecast)
library(rminer)

# Read data
cat("read walmart time series:")
dados <- read.csv("walmart.csv")
d1 <- dados$WSdep1  
L <- length(d1) 
K <- 4 

Test <- K 
S <- round(K / 1) 
Runs <- 8  # Alterado para 1 para remover a lógica de janela de crescimento

# forecast:
W <- L - Test  # Removido o cálculo baseado em Runs
W_arima <- W
W_nnetar <- W
W_ets <- W

# rminer:
timelags <- c(1, 4, 8) 
D <- CasesSeries(d1, timelags) 
W_mlpe <- W - max(timelags) 
W_lm <- W - max(timelags)

YR <- diff(range(d1))

ev_holtwinters <- vector(length = Runs) 
ev_mlpe <- vector(length = Runs) 
ev_arima <- vector(length = Runs) 
ev_nnetar <- vector(length = Runs) 
ev_ets <- vector(length = Runs) 
ev_lm <- vector(length = Runs) 
ev_naive <- vector(length = Runs)  # Adicionado para armazenar os erros do Naive

# Sem o ciclo para a janela de crescimento:
# HoltWinters
H <- holdout(d1, ratio = Test, mode = "fixed", window = W, increment = S)   
trinit <- H$tr[1]
dtr <- ts(d1[H$tr], frequency = K)
M <- suppressWarnings(HoltWinters(dtr))
Pred_holtwinters <- forecast(M, h = length(H$ts))$mean[1:Test]
ev_holtwinters <- mmetric(y = d1[H$ts], x = Pred_holtwinters, metric = "NMAE", val = YR)

# ARIMA
H_arima <- holdout(d1, ratio = Test, mode = "fixed", window = W_arima, increment = S) 
M_arima <- auto.arima(dtr)
Pred_arima <- forecast(M_arima, h = length(H_arima$ts))$mean[1:Test]
ev_arima <- mmetric(y = d1[H$ts], x = Pred_arima, metric = "NMAE", val = YR)

# nnetar
H_nnetar <- holdout(d1, ratio = Test, mode = "fixed", window = W_nnetar, increment = S) 
M_nnetar <- nnetar(dtr)
Pred_nnetar <- forecast(M_nnetar, h = length(H_nnetar$ts))$mean[1:Test]
ev_nnetar <- mmetric(y = d1[H$ts], x = Pred_nnetar, metric = "NMAE", val = YR)

# ETS
H_ets <- holdout(d1, ratio = Test, mode = "fixed", window = W_ets, increment = S) 
M_ets <- ets(dtr)
Pred_ets <- forecast(M_ets, h = length(H_ets$ts))$mean[1:Test]
ev_ets <- mmetric(y = d1[H$ts], x = Pred_ets, metric = "NMAE", val = YR)

# mlpe
H_mlpe <- holdout(D$y, ratio = Test, mode = "fixed", window = W_mlpe, increment = S)   
M_mlpe <- fit(y ~ ., D[H_mlpe$tr, ], model = "mlpe")
Pred_mlpe <- lforecast(M_mlpe, D, start = (length(H_mlpe$tr) + 1), Test)
ev_mlpe <- mmetric(y = d1[H$ts], x = Pred_mlpe, metric = "NMAE", val = YR)

# LM
H_lm <- holdout(D$y, ratio = Test, mode = "fixed", window = W_lm, increment = S)   
M_lm <- fit(y ~ ., D[H_lm$tr, ], model = "lm")
Pred_lm <- lforecast(M_lm, D, start = (length(H_lm$tr) + 1), Test)
ev_lm <- mmetric(y = d1[H$ts], x = Pred_lm, metric = "NMAE", val = YR)

# Naive
H_naive <- holdout(d1, ratio = Test, mode = "fixed", window = W, increment = S)   
Pred_naive <- numeric(length(H_naive$ts))

for (i in seq_along(H_naive$ts)) {
  # Obter o índice de início e fim da janela de treinamento
  train_start <- max(1, H_naive$ts[i] - W + 1)
  train_end <- H_naive$ts[i] - 1
  
  # Extrair os dados de treinamento para esta iteração
  train_data <- d1[train_start:train_end]
  
  # Ajustar o modelo seasonal naive aos dados de treinamento
  model <- snaive(train_data, h = 1)
  
  # Prever o próximo ponto
  Pred_naive[i] <- forecast(model)$mean[1]
}

# Avaliar as previsões do Naive
ev_naive <- mmetric(y = d1[H_naive$ts], x = Pred_naive, metric = "NMAE", val = YR)

cat("TR from:", trinit, "to:", (trinit + length(H$tr) - 1), "size:", length(H$tr),
    "TS from:", H$ts[1], "to:", H$ts[length(H$ts)], "size:", length(H$ts),
    "nmae:", ev_holtwinters, ",", ev_arima, ",", ev_nnetar, ",", ev_ets, ",", ev_mlpe, ",", ev_lm, ",", ev_naive, "\n")

# Plot graphs
mgraph(d1[H$ts], Pred_holtwinters, graph="REG", Grid=10,
       col=c("black","blue","green","purple","orange","red","pink","brown"),
       leg=list(pos="topleft",leg=c("target","HW pred.","arima","nn","ets","mlpe","lm","naive")))
lines(Pred_arima, pch=19, cex=0.5, type="b", col="green")
lines(Pred_mlpe, pch=19, cex=0.5, type="b", col="red")
lines(Pred_nnetar, pch=19, cex=0.5, type="b", col="blue")
lines(Pred_ets, pch=19, cex=0.5, type="b", col="orange")
lines(Pred_lm, pch=19, cex=0.5, type="b", col="pink")
lines(Pred_naive, pch=19, cex=0.5, type="b", col="brown")


# Apresentar as previsões
cat("Holt-Winters previsao ", Pred_holtwinters, "\n")
cat("Arima previsao ", Pred_arima, "\n")
cat("MLPE previsao ", Pred_mlpe, "\n")
cat("NNetar previsao ", Pred_nnetar, "\n")
cat("ETS previsao ", Pred_ets, "\n")
cat("LM previsao ", Pred_lm, "\n")
cat("naive previsao ", Pred_naive, "\n")


