# Librarias
source("R_functions//hill.R")
source("R_functions//FuncaoEvalFuncionar.R")
library(genalg) # Para o genetic


D=28#dimensao


Hill = function(eval, preds){ #, preds
  #Pass preds to an matrix
  vendas_matrix <- matrix(preds, nrow = 4, ncol = 4, byrow = TRUE)
  
  #print(vendas_matrix)
  
  # vendas_matrix <- matrix(c(54480,42221,36267,35283,
  #                           159460,156945,146388,132156,
  #                           63584,62888,62768,60279,
  #                           127009,124560,123346,117375),nrow =4, byrow=FALSE)
  
  order_matrix <- matrix(c(61662,0,12985,39924,
                           78292,0,55403,75160,
                           56434,0,69133,62131,
                           24182,0,37167,99708),nrow =4, byrow=FALSE)
  
  upper_funcionarios <- function(matriz_prev_vendas){
    juniores <- ceiling(matriz_prev_vendas / 4000)
    normais <- ceiling(matriz_prev_vendas / 7000)
    seniores <- ceiling(matriz_prev_vendas / 9500)
    
    # Calculando a soma das colunas
    juniores_total <- colSums(juniores)
    normais_total <- colSums(normais)
    seniores_total <- colSums(seniores)
    
    #totais <- c(Juniores = juniores_total, Normais = normais_total, Seniores = seniores_total)
    totais <- c(juniores_total, normais_total, seniores_total)
    return(totais)
  }
  
  # Função calcular o valor maximo de encomendas
  upper_encomendas <- function(matriz_encomendas){
    # Posso encomendas tudo de uma vez
    #soma <- colSums(matriz_encomendas)
    
    max_vals <- numeric(length(matriz_encomendas))
    for (col in 1:ncol(matriz_encomendas)) {
      for (row in 1:nrow(matriz_encomendas)) {
        max_vals[row + (col - 1) * nrow(matriz_encomendas)] <- sum(matriz_encomendas[row:nrow(matriz_encomendas), col])
      }
    }
    return(max_vals)
  }
  
  N=1000 # numero de iteracoes
  lower <- rep(0,D)
  upper <- rep(c(upper_funcionarios(vendas_matrix),upper_encomendas(order_matrix)))
  
  ceiling <- ceiling(runif(D,lower, upper))
  
  REPORT=N/20 # report results
  # rchasge3
  # slight change of a real par under a normal u(0,0.5) function:
  rchange1=function(ceiling,lower,upper) # change for hclimbing
  { hchange(ceiling,lower=lower,upper=upper,rnorm,mean=0,sd=0.25,round=TRUE) }
  
  
  switch(eval,
         `1` = {# Objective 1
            
           obj_1=hclimbing(par=ceiling,fn=eval1neg,change=rchange1,lower=lower,upper=upper,type="max",
                        control=list(maxit=N,REPORT=REPORT,digits=2))
           
           #cat("best solution:",HC$sol,"evaluation function",HC$eval,"\n")
           
           return(obj_1)
           
         },
         `2` = {# Objective 2
           obj_2=hclimbing(par=ceiling,fn=eval2,change=rchange1,lower=lower,upper=upper,type="max",
                        control=list(maxit=N,REPORT=REPORT,digits=2))
           
           #cat("best solution:",HC$sol,"evaluation function",HC$eval,"\n")
           
           return(obj_2)
           
         },
         `3` = {# Both
           # HC_1=hclimbing(par=ceiling,fn=eval1neg,change=rchange1,lower=lower,upper=upper,type="max",
           #              control=list(maxit=N,REPORT=REPORT,digits=2))
           # 
           # HC_2=hclimbing(par=ceiling,fn=eval2neg,change=rchange1,lower=lower,upper=upper,type="max",
           #                control=list(maxit=N,REPORT=REPORT,digits=2))
           
           #result <- c(HC_1, HC_2)
           
           result <- list(
             obj_1 = hclimbing(par = ceiling, fn = eval1neg, change = rchange1, lower = lower, upper = upper, type = "max",
                              control = list(maxit = N, REPORT = REPORT, digits = 2)),
             obj_2 = hclimbing(par = ceiling, fn = eval2neg, change = rchange1, lower = lower, upper = upper, type = "max",
                              control = list(maxit = N, REPORT = REPORT, digits = 2))
           )
           
           #cat("best solution:",result$HC_1$,"evaluation function",HC$eval,"\n")
           
           return(result)
           
         },
         {
           print("Unknown model")
           return("Unknown model")
         }
  )
  
  return("Null")
  
}
