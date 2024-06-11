source("R_functions//hill.R")
source("R_functions//FuncaoEvalFuncionar.R")


# dimension
D=28

# Function to Sann
# Eval, qual eval vai escolher
# Preds, predictions que fez anteriormente
Sann = function(eval, preds){#
  #Pass preds to an matrix
  vendasAtuais <- matrix(preds, nrow = 4, ncol = 4, byrow = TRUE)
  
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
  
  # slight change of a real par under a normal u(0,0.5) function:
  rchange2=function(par) # change for hclimbing
  { hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.5,round=FALSE) }
  
  #cat("Simulated Annealing search sphere D=",D,"(iters=",N,")\n")
  CSANN=list(maxit=N,temp=5,trace=TRUE)
  x=c(5,6,7,4,5,6,3,4,5,2,3,4,61662,0,12985,39924,78292,0,55403,75160,56434,0,69133,62131,24182,0,37167,99708)
  
  #MC=mcsearch(fn=eval1,lower=lower,upper=upper,N=1,type="max")
  #x = MC$sol
  #print(x)
  
  switch(eval,
         `1` = {# Objective 1
           
           obj_1=optim(par=x,fn=eval1neg,method="SANN",gr=rchange2,control=CSANN)
           #par manda os parametros , o eval manda a funçao eval com os parametros
           #cat("best solution:",SA$par,"evaluation function",SA$value,"\n")
           
           obj_1$sol <- obj_1$par
           
           return(obj_1)
           
         },
         `2` = {# Objective 2
           obj_2=optim(par=x,fn=eval2neg,method="SANN",gr=rchange2,control=CSANN)
           #par manda os parametros , o eval manda a funçao eval com os parametros
           #cat("best solution:",SA$par,"evaluation function",SA$value,"\n")
           
           obj_2$sol <- obj_2$par
           
           return(obj_2)
           
           
         },
         `3` = {# Both, not working
           result <- list(
             obj_1 = optim(par=x,fn=eval1neg,method="SANN",gr=rchange2,control=CSANN),
             obj_2 = optim(par=x,fn=eval2neg,method="SANN",gr=rchange2,control=CSANN)
           )
           
           print("asdasdsad")
           print(result[1])
           print("asdasdsad")
           
           result$obj_1$sol <- result$obj_1$par
           result$obj_2$sol <- result$obj_2$par
           
           #par manda os parametros , o eval manda a funçao eval com os parametros
           #cat("best solution:",SA$par,"evaluation function",SA$value,"\n")
           
           return(result)
           
           
         },
         {
           print("Unknown model")
           return("Unknown model")
         }
  )
  
}

#Sann(3)
