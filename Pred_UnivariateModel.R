# Load necessary libraries
library(forecast)
library(rminer)
library(shiny)

# Source other files
source("R_functions//holtwinters_gw.R")
source("R_functions//arima_gw.R")
source("R_functions//mlpe_gw.R")
source("R_functions//nnetar_gw.R")
source("R_functions//ets_gw.R")
source("R_functions//lm_gw.R")
source("R_functions//naive_gw.R")
source("R_functions//rf_gw.R")

# Read data
dados <- read.csv("Data//walmart.csv",sep=",",header=TRUE)
d1 <- dados$WSdep1  
L <- length(d1) 
K <- 4 

# Set parameters
Test <- K 
S <- round(K/1) 
Runs <- 8 

W <- (L-Test)-(Runs-1)*S 
W_arima <- (L-Test)-(Runs-1)*S
W_nnetar <- (L-Test)-(Runs-1)*S
W_ets <- (L-Test)-(Runs-1)*S

timelags <- c(1,4,8) 
D <- CasesSeries(d1,timelags) 
W_mlpe <- W-max(timelags) 
W_lm <- W-max(timelags)

YR <- diff(range(d1)) 

UnivariateModel <- function(uni_model, month, dep) {
  
  uni_model <- as.double(uni_model)
  month <- as.integer(month)
  # Calculate the subset length
  #subset_length <- (month - 1) * 4 + 1
  subset_length <- month * 4
  
  print(uni_model)
  #d_subset <- dados
  d_subset <- dados[1:subset_length,]
  
  d_subset <- switch(dep,
                     '1' = d_subset$WSdep1,
                     '2' = d_subset$WSdep2,
                     '3' = d_subset$WSdep3,
                     '4' = d_subset$WSdep4)
  
  # Common parameters
  Test <- K 
  S <- round(K/1)
  Runs <- 8  # Assuming Runs is defined here as well
  
  # Calculate window size
  W <- (length(d_subset) - Test) - (Runs - 1) * S
  W_arima <- W
  W_nnetar <- W
  W_ets <- W
  
  timelags <- c(1, 4, 8)
  D <- CasesSeries(d_subset, timelags)
  W_mlpe <- W - max(timelags)
  W_lm <- W - max(timelags)
  
  YR <- diff(range(d_subset))
  
  ev <- vector(length=Runs) 
  mae <- vector(length=Runs) 
  rmse <- vector(length=Runs) 
  r2 <- vector(length=Runs)
  
  predictions <- data.frame(
    Run = integer(Runs),
    Model = character(Runs),
    Pred = I(vector("list", Runs)),
    ev = numeric(Runs),
    mae = numeric(Runs),
    rmse = numeric(Runs),
    r2 = numeric(Runs),
    stringsAsFactors = FALSE
  )
  
  
  for(b in 1:Runs)  {
    
    H <- holdout(d_subset, ratio=Test, mode="incremental", iter=b, window=W, increment=S) 
    
    if (uni_model == 1) {
      method_name <- "Holt-Winters"# Colocar nome do metodo
      results <- holtwinters_gw(d_subset, Test, W, S, b, H)
      
    } else if (uni_model == 2) {
      
      method_name <- "ARIMA"# Colocar nome do metodo
      results <- arima_gw(d_subset, Test, W_arima, S, b, H)
      
    } else if (uni_model == 3) {
      
      method_name <- "MLPE"# Colocar nome do metodo
      results <- mlpe_gw(D, Test, W_mlpe, S, b)
      
    } else if (uni_model == 4) {
      
      method_name <- "NNETAR"# Colocar nome do metodo
      results <- nnetar_gw(d_subset, Test, W_nnetar, S, b)
      
    } else if (uni_model == 5) {
      
      method_name <- "ETS"# Colocar nome do metodo
      results <- ets_gw(d_subset, Test, W_ets, S, b)
      
    } else if (uni_model == 6) {
      
      method_name <- "LM"# Colocar nome do metodo
      results <- lm_gw(D, Test, W_lm, S, b)
      
    } else if (uni_model == 7) {
      
      method_name <- "NAIVE"# Colocar nome do metodo
      results <- naive_gw(D, Test, W_lm, S, b)
      
    } else if (uni_model == 8) {
      
      method_name <- "Random Forest"# Colocar nome do metodo
      results <- rf_gw(D, Test, W_lm, S, b)
      
    } 
  }
  
  
  return(results)
  
}

# Function to retrieve data for the selected month
get_month_data <- function(selected_month) {
  # Calculate the starting row index for the selected month
  start_row <- (selected_month-1) * 4 + 1
  # Calculate the ending row index
  end_row <- start_row + 3
  # Extract the corresponding rows from the dataset
  month_data <- walmart[start_row:end_row, ]
  return(month_data)
}



