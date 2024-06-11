library(tidyverse)
library(rminer)
library(forecast)




getWeek = function(weekNumber){
  init = 36
  resInit <- 7 * (weekNumber-1) + init
  resFinal <- resInit + 6
  res <- c(resInit:resFinal)
  return(res)
}

getWeekDays = function(week){
  res = dates[week,]
  return(res)
}


initVars = function(){
  # dates <<- read.csv(file = './extra/date.csv')["x"]
  # all_data <<- read.csv(file = './extra/TodosDadosTab.csv')
  all_data <- read.csv(file = './Data/walmart.csv')
  #all_data <<- subset (all_data, select = -X)
  timeSeries <<- c("all","department 1","department 2","department 3","department 4") ##
  timeSeries_list <<- list("all"=1,"department 1"=2,"department 2"=3,"department 3"=4,"department 4"=5) ##
  models_rminer <<- c("lm","mlpe","naive","ctree","mlp","randomForest","mr","rvm","ksvm")
  models_forecast <<- c("HW","Arima","NN","ETS")
  weeks <- list(
    "Week 6" = 1,
    "week 7" = 2,
    "week 8" = 3,
    "week 9" = 4,
    "week 10" = 5,
    "week 11" = 6,
    "week 12" = 7,
    "week 13"= 8,
    "week 14" = 9,
    "week 15" = 10,
    "week 16" = 11,
    "week 17" = 12,
    "week 18" = 13,
    "week 19" = 14,
    "week 20" = 15,
    "week 21" = 16,
    "week 22" = 17,
    "week 23" = 18,
    "week 24" = 19,
    "week 25" = 20,
    "week 26" = 21,
    "week 27" = 22,
    "week 28" = 23,
    "week 29" = 24,
    "week 30" = 25,
    "week 31" = 26,
    "week 32" = 27,
    "week 33" = 28,
    "week 34" = 29,
    "week 35" = 30,
    "week 36" = 31
  )

  print("Variables Initialized")
}

initVars()


# UnivariateModel(uni_model,week){
#   ##### something
#   
# }
# 
# 
# MultivariateModel(multi_model,week){
#   ##### something
# }
# 
# 
# BestModel = function(week){
#   ##### something
# }


#----------- Optimization  ----------------#


Optimization = function(opt_model,preds,obj){
  print("init optimizaion")
  createPreds(preds)
  switch(  
    opt_model,  
    "HillClimb"= {print("hillclimb"); res = hill(obj)},
    "MonteCarlo"= {print("mc"); res = montecarlo(obj)},
    "Tabu"= {print("tabu"); res = tabu(obj)},
    "Sann"= {print("sann"); res = sann(obj)},
    "RBGA" = {print("rgba"); res = rbgaFunction(obj)},
    "DeOptim" = {print("deoptim"); res = DEoptimFunction(obj)},
    "PsOptim" = {print("psoptim"); res = psoptimFunction(obj)}
  )
  return(res)
}