source("R_functions//blind.R") # fsearch is defined here
source("R_functions//montecarlo.R") # mcsearch is defined here
source("R_functions//FuncaoEvalFuncionar.R")
 
# dimension
D=28

# Function to montecarlo
# Eval, qual eval vai escolher
# Preds, predictions que fez anteriormente
Montecarlo <- function(eval, preds){#
  #Pass preds to an matrix
  vendasAtuais <- matrix(preds, nrow = 4, ncol = 4, byrow = TRUE)
  
  # Vendas Atuais da Walmart
  # vendasAtuais <- matrix(c(54480, 159460, 63584, 127009,
  #                          42221, 156945,62888, 124560,
  #                          36267, 146388, 62768, 123346,
  #                          35283, 132156, 60279, 117375),
  #                        nrow = 4, ncol = 4, byrow = TRUE)
  
  # calcular limite maximo de funcionários de cada tipo segundo as vendas previstas
  upperFuncionarios <- function(vendasPrevMatrix){
    juniores <- ceiling(vendasPrevMatrix / 4000)
    normais <- ceiling(vendasPrevMatrix / 7000)
    seniores <- ceiling(vendasPrevMatrix / 9500)
    
    # Calculando a soma das colunas
    TotalDeJuniores <- colSums(juniores)
    TotalDeNormais <- colSums(normais)
    TotalDeSenioress <- colSums(seniores)
    
    #totais <- c(Juniores = juniores_total, Normais = normais_total, Seniores = seniores_total)
    MaximoFuncionarios <- c(TotalDeJuniores, TotalDeNormais, TotalDeSenioress)
    return(MaximoFuncionarios)
  }
  
  #Função calcular o valor maximo de encomendas
  upperEncomendas <- function(vendasAtuais){
    # Posso encomendas tudo de uma vez
    #soma <- colSums(vendasAtuais)
    
    MaxVendasAtuais <- numeric(length(vendasAtuais))
    for (col in 1:ncol(vendasAtuais)) {
      for (row in 1:nrow(vendasAtuais)) {
        MaxVendasAtuais[row + (col - 1) * nrow(vendasAtuais)] <- sum(vendasAtuais[row:nrow(vendasAtuais), col])
      }
    }
    return(MaxVendasAtuais)
  }
  
  # evaluation function:
  N=100 # number of searches
  # monte carlo search with D=2 and x in [-10.4,10.4]
  lower=rep(0,D) # lower bounds
  upper=rep(c(upperFuncionarios(vendasAtuais), upperEncomendas(vendasAtuais)))
  
  switch(eval,
         `1` = {# Objective 1
           obj_1=mcsearch(fn=eval1,lower=lower,upper=upper,N=N,type="max")
           #cat("best solution 1:",round(MC$sol),"evaluation function",MC$eval," (found at iteration:",MC$index,")\n")
           
           return(obj_1)
           
         },
         `2` = {# Objective 2
           obj_2=mcsearch(fn=eval2,lower=lower,upper=upper,N=N,type="max")
           #cat("best solution 2:",round(MC$sol),"evaluation function",MC$eval," (found at iteration:",MC$index,")\n")

           return(obj_2)
           
         },
         `3` = {# Both, not working
           
           result <- list(
             obj_1 = mcsearch(fn=eval1,lower=lower,upper=upper,N=N,type="max"),
             obj_2 = mcsearch(fn=eval2,lower=lower,upper=upper,N=N,type="max")
           )
           #cat("best solution both:",round(MC$sol),"evaluation function",MC$eval," (found at iteration:",MC$index,")\n")
           
           return(result)
           
         },
         {
           #print("Unknown model")
           return("Unknown model")
         }
  )
  
}

#Montecarlo(2)

