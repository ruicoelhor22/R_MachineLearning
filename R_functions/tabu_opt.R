source("R_functions//FuncaoEvalFuncionar.R")

library(tabuSearch)
library(adana)



# Global variables (can be used inside the functions):
D <- 28 # dimension
N <- 10 # number of iterations


# Function to convert binary to integer
bin2int <- function(binary_vector) {
  sum(binary_vector * 2^(rev(seq_along(binary_vector) - 1)))
}

# Function to divide binary array by bits 
matrix_transform <- function(solution, start, elements, bits){
  matrix_final <- c()
  for(i in 1:elements){
    matrix_final[i] <- bin2int(solution[start:(start + bits - 1)])
    start <- start + bits
  }
  return(matrix_final)
}

# Function to calculate the overall values used in evaluations
calculate_values <- function(solution, bits_workers, bits_orders) {
  print(bits_workers)
  print(bits_orders)
  empregados <- matrix(matrix_transform(solution, start = 1, elements = 12, bits = bits_workers), nrow = 3, ncol = 4)
  encomendas <- matrix(matrix_transform(solution, start = 12 * bits_workers + 1, elements = 16, bits = bits_orders), nrow = 4, ncol = 4)
  
  # Chamar a função eval do arquivo FuncaoEvalFuncionar.R para calcular os valores
  results <- eval(c(as.vector(empregados), as.vector(encomendas)))
  
  # Extrair valores calculados
  f1 <- results[1]
  f2 <- results[2]
  
  vendasEfetivas <- calcVendasEfetivas(vendasPrevistas, encomendas, calcEmpregadosMaxSup(empregados, capacidadeEmpregados))
  
  return(list(monthly_profit = f1, monthly_effort = f2, sales = vendasEfetivas, empregados = empregados, encomendas = encomendas))
}

# Evaluation Function
eval <- function(solution) {
  values <- calculate_values(solution)
  return(values)
}

eval1 <- function(solution) {
  values <- eval(solution)
  return(values$monthly_profit) #
}

# Evaluation Function F2
F2 <- function(solution) {
  values <- eval(solution)
  return(-values$monthly_effort) # needs to be negative because tabuSearch only maximizes
}

# Evaluation Function Multi Objective
multi_eval <- function(solution) {
  values <- eval(solution)
  #print(values)
  return(values[1] - values[2])
}

# Function to build Initial Config
initial_config_build <- function(config, n_bits, dimensions) {
  initial_length <- length(config)
  while (length(config) - initial_length < dimensions * n_bits) {
    config <- c(config, rep(0, n_bits), rep(1, n_bits))
  }
  return(config)
}

# Calculating the maximum limit of employees of each type according to the forecasted sales
upperFuncionarios <- function(vendasPrevMatrix) {
  juniores <- ceiling(vendasPrevMatrix / 4000)
  normais <- ceiling(vendasPrevMatrix / 7000)
  seniores <- ceiling(vendasPrevMatrix / 9500)
  TotalDeJuniores <- colSums(juniores)
  TotalDeNormais <- colSums(normais)
  TotalDeSeniores <- colSums(seniores)
  MaximoFuncionarios <- c(TotalDeJuniores, TotalDeNormais, TotalDeSeniores)
  return(MaximoFuncionarios)
}

# Function to calculate the maximum value of orders
upperEncomendas <- function(vendasAtuais) {
  MaxVendasAtuais <- numeric(length(vendasAtuais))
  for (col in 1:ncol(vendasAtuais)) {
    for (row in 1:nrow(vendasAtuais)) {
      MaxVendasAtuais[row + (col - 1) * nrow(vendasAtuais)] <- sum(vendasAtuais[row:nrow(vendasAtuais), col])
    }
  }
  return(MaxVendasAtuais)
}


Tabu = function(eval, preds){
  
  vendasAtuais <- matrix(preds, nrow = 4, ncol = 4, byrow = TRUE)
  
  Low <- rep(0, D) # Lower bounds
  Up <- c(upperFuncionarios(vendasAtuais), upperEncomendas(vendasAtuais)) # Upper bounds
  
  bits_workers <- ceiling(max(log2(Up[1:12]))) # Bits for Hired Workers
  bits_orders <- ceiling(max(log2(Up[13:28]))) # Bits for Product Orders
  
  size <- 12 * bits_workers + 16 * bits_orders # solution size
  
  #Make configs
  initial_config <- c()
  # Building Initial configuration for Hired Workers
  initial_config <- initial_config_build(config = initial_config, n_bits = bits_workers, dimensions = 12)
  # Building Initial configuration for Product Orders
  initial_config <- initial_config_build(config = initial_config, n_bits = bits_orders, dimensions = 16)
  
  switch(eval,
         `1` = {# Objective 
           #obj_1=optim(par=x,fn=eval1neg,method="SANN",gr=rchange2,control=CSANN)
           solution <- tabuSearch(size, iters = N, objFunc = eval1, config = initial_config, verbose = TRUE)
           b <- which.max(solution$eUtilityKeep) # best index
           bs <- solution$configKeep[b, ]
           
           obj_1 <- calculate_values(bs, bits_workers, bits_orders)
          
           #Create sol like we need in shiny
           obj_1$sol <- c(empregados, encomendas)
           
           return(obj_1)
           
         },
         `2` = {# Objective 2
           solution <- tabuSearch(size, iters = N, objFunc = multi_eval, config = initial_config, verbose = TRUE)
           b <- which.max(solution$eUtilityKeep) # best index
           bs <- solution$configKeep[b, ]
           
           obj_2 <- calculate_values(bs, bits_workers, bits_orders)
           
           #Create sol like we need in shiny
           obj_2$sol <- c(empregados, encomendas)
           
           return(obj_2)
         },
         `3` = {# Both
           solution_1 <- tabuSearch(size, iters = N, objFunc = eval1, config = initial_config, verbose = TRUE)
           b_1 <- which.max(solution_1$eUtilityKeep) # best index
           bs_1 <- solution_1$configKeep[b_1, ]
           
           obj_1 <- calculate_values(bs_1, bits_workers, bits_orders)
           
           #Create sol like we need in shiny
           obj_1$sol <- c(empregados, encomendas)
           
           solution_2 <- tabuSearch(size, iters = N, objFunc = multi_eval, config = initial_config, verbose = TRUE)
           b_2 <- which.max(solution_2$eUtilityKeep) # best index
           bs_2 <- solution_2$configKeep[b_2, ]
           
           obj_2 <- calculate_values(bs_2, bits_workers, bits_orders)
           
           #Create sol like we need in shiny
           obj_2$sol <- c(empregados, encomendas)
           
           return(list(obj_1, obj_2))
         },
         {
           print("Unknown model")
           return("Unknown model")
         }
  )
}