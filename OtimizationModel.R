## File that contains all otimization for shiny

#Sources
source("R_functions//montecarlo_opt.R")
source("R_functions//sann_opt.R")
source("R_functions//tabu_opt.R")
source("R_functions//hill_opt.R")

#Function to call optimization models
# opt_model, number
  # 1 - Montecarlo
  # 2 - Sann
  # 3 - Tabu
  # 4 - Hill
# objective, number
  # 1 - Objective 1
  # 2 - Objective 2
  # 3 - Both
# Preds, vector with predictions
Optimization <- function(opt_model, objective, preds){
  
  #print(preds)
  
  switch(opt_model,
         `1` = {# Montecarlo
           result <- Montecarlo(as.numeric(objective), preds)
           return(result)
         },
         `2` = {# Sann
           result <- Sann(as.numeric(objective), preds)
           return(result)
         },
         `3` = {# Tabu
           result <- Tabu(as.numeric(objective), preds)
           
           return(result)
         },
         `4` = {# Hill
           result <- Hill(as.numeric(objective), preds)
           return(result)
         },
         {
           return("Unknown model")
         }
  )
}

#Just for test
#Optimization(1,1)
