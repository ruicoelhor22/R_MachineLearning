#Vendas previstas (Neste momento correspondem às ultimas 4 semanas)
d=read.table("Data//walmart.csv",header=TRUE,sep=",")

vendasPrevistas <- d[, 4:7][nrow(d) - 3:0, ]

#cat("Vendas Previstas:\n")
#print(vendasPrevistas)

#Solução empregados
#linhas -> tipo ; colunas -> dep
empregados <- c(5,6,7,4,5,6,3,4,5,2,3,4)

mEmpregados <- matrix(data = empregados, nrow = 3, ncol = 4)
#cat("Solução de Empregados:\n")
#print(mEmpregados)

#Solução encomendas
#linhas -> semanas
encomendas <- c(61662,0,12985,39924,  78292, 0, 55403, 75160,   56434, 0, 69133, 62131,   24182,  0,  37167, 99708)
mEncomendas <- matrix(data = encomendas, nrow = 4, ncol = 4)
#cat("Solução de Encomendas:\n")
#print(mEncomendas)



#Calculo custo total empregados
custoEmpregados <- c(6000, 8000, 9750)

calcCustoEmpregados <- function(empregados){
  mEmpregados <- matrix(data = empregados, nrow = 3, ncol = 4)
  resultados <- vector("numeric", length = 3)
  
  for (i in 1:nrow(mEmpregados)) {
    soma_linha <- sum(mEmpregados[i, ])
    resultados[i] <- soma_linha * custoEmpregados[i]
  }
  return(sum(resultados))
}

#Calculo nº produtos que funcionarios suportam p//Dep

capacidadeEmpregados <- c(4000, 7000, 9500)

calcEmpregadosMaxSup <- function(empregados, capacidadeEmpregados) {
  mEmpregados <- matrix(data = empregados, nrow = 3, ncol = 4)
  resultado <- numeric(ncol(mEmpregados))
  for (j in 1:ncol(mEmpregados)) {
    soma <- 0
    for (i in 1:nrow(mEmpregados)) {
      soma <- soma + mEmpregados[i, j] * capacidadeEmpregados[i]
    }
    resultado[j] <- soma
  }
  
  return(resultado)
}

#Calculo custo total encomendas
custoEncomendas <- c(6, 8, 9, 11)

calcCustoEncomendas <- function(encomendas){
  mEncomendas <- matrix(data = encomendas, nrow = 4, ncol = 4)
  resultados <- vector("numeric", length = 4)
  
  for (i in 1:nrow(mEncomendas)) {
    soma_col <- sum(mEncomendas[, i])
    resultados[i] <- soma_col * custoEncomendas[i]
  }
  return(sum(resultados))
}


#Calculo matriz de vendas efetivas
#Vendas efetivas = previstas SE previstas<=encomenda E previstas<=sup funcionarios
#Vendas efetivas = min(Encomendas, Funcionarios) SE previstas>encomendas OU previstas>sup func

calcVendasEfetivas <- function(vendasPrevistas, mEncomendas, capacidadeEmpregados) {
  # Inicializar uma matriz para armazenar as vendas efetivas
  vendasEfetivas <- matrix(0, nrow = nrow(vendasPrevistas), ncol = ncol(vendasPrevistas))
  
  # Calcular as vendas efetivas com base nas condições especificadas
  for (i in 1:nrow(vendasPrevistas)) {
    for (j in 1:ncol(vendasPrevistas)) {
      if (vendasPrevistas[i, j] <= mEncomendas[i, j] & vendasPrevistas[i, j] <= capacidadeEmpregados[j]) {
        vendasEfetivas[i, j] <- vendasPrevistas[i, j]
      } else if (i > 1 && vendasPrevistas[i - 1, j] <= mEncomendas[i - 1, j]) {
        vendasEfetivas[i, j] <- mEncomendas[i-1, j] - vendasPrevistas[i - 1, j]
      } else {
        vendasEfetivas[i, j] <- min(mEncomendas[i, j], capacidadeEmpregados[j])
      }
    }
  }
  # Retornar a matriz de vendas efetivas
  return(vendasEfetivas)
}

#Calculo matriz de ganhos
valorProdutos <- c(8, 10, 12, 16)
calcVendasUSD <- function(vendasEfetivas, valorProdutos) {
  vendasUSD <- vendasEfetivas
  for (i in 1:ncol(vendasEfetivas)) {
    vendasUSD[, i] <- vendasEfetivas[, i] * valorProdutos[i]
  }
  return(vendasUSD)
}

#Calculo matriz stock 
calcStock <- function(vendasEfetivas, mEncomendas) {
  stock <- matrix(0, nrow = 4, ncol = 4)  # Estoque inicialmente zero
  # Calcular o estoque para cada semana e departamento
  for (s in 1:4) {
    for (d in 1:4) {
      if (s == 1) {
        # Para a primeira semana, só se consideram as encomendas e vendas
        stock[s, d] <- mEncomendas[s, d] - vendasEfetivas[s, d]
      } else {
        # Para as demais semanas, o estoque é o estoque anterior + encomendas - vendas
        stock[s, d] = stock[s - 1, d] + mEncomendas[s, d] - vendasEfetivas[s, d]
      }
    }
  }
  return(stock)
}


#Calculo matriz custo stock
custoProdutosStock <- c(3, 5, 6, 8)
calcCustoStock <- function(stock, custoProdutosStock) {
  custoStock <- matrix(0, nrow = 4, ncol = 4)
  for (i in 1:4) {
    custoStock[, i] <- stock[, i] * custoProdutosStock[i]
  }
  return(custoStock)
}

eval <- function(s){
  s <- round(s)
  # Divide s into employees and orders
  empregados = s[1:12]
  encomendas = s[13:28]
  
  # Use each of the functions to calculate everything
  custoEmpregados <- calcCustoEmpregados(empregados)
  custoEncomendas <- calcCustoEncomendas(encomendas)
  capacidadeEmpregados <- calcEmpregadosMaxSup(empregados, capacidadeEmpregados)
  numEncomendas <- sum(encomendas != 0)
  vendasEfetivas <- calcVendasEfetivas(vendasPrevistas, mEncomendas, capacidadeEmpregados) # Fixed function name here
  
  # cat("\nCusto total de empregados:", custoEmpregados)
  # cat("\nNumero total de empregados:", sum(empregados))
  # cat("\nCapacidade de suporte p/Dep:", capacidadeEmpregados)
  # cat("\nCusto de encomendas:", custoEncomendas)
  # cat("\nNumero de encomendas:", numEncomendas)
  # cat("\nVendas Efetivas: \n")
  # print(vendasEfetivas)
  # cat("\nVendas USD: \n")
  vendasUSD <- calcVendasUSD(vendasEfetivas, valorProdutos)
  # print(vendasUSD)
  # cat("\nGanho total:", sum(vendasUSD))
  # cat("\nStock: \n")
  stock <- calcStock(vendasEfetivas, mEncomendas)
  # print(stock)
  # cat("\nCusto Produtos em Stock: \n")
  custoStock <- calcCustoStock(stock, custoProdutosStock)
  # print(custoStock)
  # cat("\nCusto stock total:", sum(custoStock))
  # cat("\nCusto Total:", sum(custoEmpregados, custoEncomendas, sum(custoStock)))
  
  f1 <- sum(vendasUSD) - sum(custoEmpregados, custoEncomendas, sum(custoStock))
  f2 <- sum(numEncomendas, sum(empregados))
  
  return(c(f1,f2))
  #cat("\nF1 (month profit):", f1)
  #cat("\nF2 (month effort):", f2)
  # Returns results
}
evalneg=function(s)  -1*eval(s)

eval1=function(s) eval(s)[1]
eval1neg=function(s) -1*eval(s)[1]
eval2=function(s) eval(s)[2]
eval2neg=function(s) -1*eval(s)[2]


#Teste codigo
# s = c(empregados, encomendas)
# cat("\nRESULTADOS: \n")
# eval(s)