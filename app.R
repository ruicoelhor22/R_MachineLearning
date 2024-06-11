library(shiny)
library(shinythemes)
library(bslib)
library(DT)
library(data.table)

####-------------------SOURCES-------------------####  

source("Pred_UnivariateModel.R")
source("Pred_MultivariateModel.R")
source("OtimizationModel.R")

####-------------------VARS-------------------####  

#list of months
weeks <- list()
for (i in 10:35) {
  weeks[[paste("Month", i+1)]] <- i
}

weeks_2 <- list()
for (i in 12:35) {
  weeks_2[[paste("Month", i+1)]] <- i
}


walmart <<- read.csv("Data//walmart.csv")

objs <- c("Both", "Objective 1 - Profit", "Objective 2 - Manual effort")
opt_models <- c("HillClimb","MonteCarlo","Tabu","Sann")
rminer <- c("lm","mlpe","naive","randomForest")
forecast <- c("HW","Arima","NNETAR","ETS")
multi <- c("ARIMAX", "MultiHW", "VAR")
timeSeries_list <<- list("all"=1,"Dept 1"=2,"Dept 2"=3,"Dept 3"=4,"Dept 4"=5)



####-------------------UI-------------------####  

# Define UI
ui <- navbarPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  "TIAPOSE",
  tabPanel("Home",
           fluidPage(
             titlePanel("Home"),
             sidebarLayout(
               sidebarPanel(
                 tabsetPanel(
                   id = "mainPage",
                   tabPanel(
                     "Best Solution",
                     h2("Best Solution"),
                     selectInput("week_best", "Month", choices = weeks),
                     selectInput("obj_best", "Optimization Objective", choices = objs),
                     actionButton("predict_btn_best", "Run Model"),
                     actionButton("showHelpModal", icon("question"), style = "background-color:#00b4d8; border-color:#00b4d8")
                   ),
                   tabPanel("Univariate",
                            h3("Prediction (Univariate Modeling)"),
                            selectInput("week_uni",h5("Select the month to predict"),choices = weeks),
                            h4("Univariate Models"),
                            radioButtons("radio_uni_uni", label = h5("Forecasting Pacakge"),choices = list("Forecast" = 1, "Rminer" = 2) ,selected = 1),
                            selectInput("unimodel_uni","package",choices = c(), selected = 1),
                            br(),
                            h3("Optimization"),
                            selectInput("optmodel_uni",label = h4("Optimization Model"),choices = opt_models),
                            selectInput("obj_uni",label = h4("Objetivo de Otimização"), choices = objs),
                            actionButton("predict_btn_uni", label = "Run Model"),
                            actionButton("showHelpModal_uni", label = icon("question"), style="background-color:#00b4d8; border-color:#00b4d8"),
                            
                   ),
                   tabPanel("Multivariate",
                            h3("Prediction (Multivariate Modeling)"),
                            selectInput("week_multi",h5("Month"),choices = weeks),
                            h4("Multivariate Models"),
                            selectInput("multimodel_multi",h5("Model"),choices = multi),
                            br(),
                            h3("Optimization"),
                            selectInput("optmodel_multi",label = h4("Optimization Model"),choices = opt_models),
                            selectInput("obj_multi",label = h4("Objetivo de Otimização"), choices = objs),
                            actionButton("predict_btn_multi", label = "Run Model"),
                            actionButton("showHelpModal_multi", label = icon("question"), style="background-color:#00b4d8; border-color:#00b4d8"),
                            
                   )
                 )
               ),
               #####-------------------MAINPANEL-------------------####  
               mainPanel(
                 tabsetPanel(
                   tabPanel("Predictions", 
                            fluidPage(
                              h3(style = "text-align: center;", textOutput("model_name")),
                              div(dataTableOutput("pred_table"),dataTableOutput("table_multi"), plotOutput("graph")),#, plotOutput("graph_multi")
                              
                              h3(style = "text-align: center;", textOutput("metrics")),
                              dataTableOutput("metrics_table"),
                              div(dataTableOutput("metrics_table"), style = "padding-bottom: 50px;")
                            )),
                   tabPanel("Otimization", 
                            fluidPage(
                              h3(style = "text-align: center;", textOutput("opt_name")),
                              div(
                                h4(style = "text-align: center;", "Objective 1: Employee Effort"),
                                dataTableOutput("opt_table_func")
                              ),
                              div(
                                h4(style = "text-align: center;", "Objective 1: Optimized values for each department"),
                                dataTableOutput("opt_table_opt")
                              ),
                              div(
                                h4(style = "text-align: center;", "Objective 2: Employee Effort"),
                                dataTableOutput("opt_table_func_2")
                              ),
                              div(
                                h4(style = "text-align: center;", "Objective 2: Optimized values for each department"),
                                dataTableOutput("opt_table_opt_2")
                              ),
                              h4(style = "text-align: left;", textOutput("obj1")),
                              h4(style = "text-align: left;", textOutput("obj2")),
                              h4(style = "text-align: left;", textOutput("obj3")))
                   ),
                 )
               )
             )
           )
  ),
  
  tabPanel("Data",
           dataTableOutput("all_data")),
  
  tabPanel("Models",
           includeMarkdown("markdown/models.md")),
  
  tabPanel("About",
           titlePanel("About"),
           fluidRow(
             column(
               width = 12,
               includeMarkdown("markdown/best_models.md")
             )
           )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  output$all_data <- renderDataTable(walmart)
  
  
  observeEvent(input$showHelpModal_uni, {
    showHelpModal()
  })
  observeEvent(input$showHelpModal_multi, {
    showHelpModal()
  })
  observeEvent(input$showHelpModal, {
    showHelpModal()
  })
  observeEvent(input$closeModalBtn, {
    removeModal()
  })
  
  
  
  showHelpModal = function(){
    showModal(modalDialog(
      title = tags$span("Information",
                        tags$button(class = "btn btn-outline-dark",
                                    onclick = "Shiny.setInputValue('closeModalBtn', true)",
                                    HTML("&times;"))
      ),
      fluidPage(
        h2("Why are the months starting at month 11?"),
        h5("In order for us to train our models properly we need to have some data (i.e weeks 1-10) reserved to be train data"),
        h2("What are de Optimization Objectives?"),
        h3("Objective 1"),
        h5("Maximize Profit."),
        h3("Objective 2"),
        h5("Minimize manual labour."),
        h2("What is our best solution?"),
        h4("Our best solution is Random Forest overall."),
        h5("(These models are backed up by our model results in the Results tab)"),
        h3("Optimization Model"),
        h4("Tabu"),
        h5("All of the optimization models return great values but the best one, in our tests, was Tabu")
      ),
      easyClose = TRUE
    ))
  }
  
  
  observeEvent(input$radio_uni_uni,{
    if(input$radio_uni_uni == 1){
      updateSelectInput(session, "unimodel_uni", "(Forecast)", forecast)
    }else {
      updateSelectInput(session, "unimodel_uni", "(Rminer)", rminer)
    }
  })
  
  observeEvent(input$unimodel_uni, {
    if(identical(tolower(input$unimodel_uni), tolower("lm"))){
      updateSelectInput(session, "week_uni", choices = weeks_2)
    } else if(identical(tolower(input$unimodel_uni), tolower("mlpe"))){
      updateSelectInput(session, "week_uni", choices = weeks_2)
    } else {
      updateSelectInput(session, "week_uni", choices = weeks)
    }
  })
  
  ####-------------------UNIVARIATE-------------------####  
  
  observeEvent(input$predict_btn_uni,{
    #Getting the variables
    uni_model = input$unimodel_uni
    month = as.integer(input$week_uni)
    
    #Create another var to send to ui
    used_model <- input$unimodel_uni
    
    #Send to ui
    output$model_name <- renderText({
      paste("Model:", used_model)
    })
    
    #Make Metrics only appear when button is clicked
    output$metrics <- renderText({
      paste("Metrics")
    })
    
    uni_model_number <- switch(uni_model,
                               'HW' =  1,
                               'Arima' = 2,
                               'mlpe' = 3,
                               'NNETAR' = 4,
                               'ETS' = 5,
                               'lm' = 6,
                               'naive' = 7,
                               'randomForest' = 8)
    
    res_model_d1 <- UnivariateModel(as.double(uni_model_number), as.double(month), 1)
    res_model_d2 <- UnivariateModel(as.double(uni_model_number), as.double(month), 2)
    res_model_d3 <- UnivariateModel(as.double(uni_model_number), as.double(month), 3)
    res_model_d4 <- UnivariateModel(as.double(uni_model_number), as.double(month), 4)
    
    month_data <- get_month_data(as.integer(month))
    
    # Create a data frame with the results
    results <- data.table(
      Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
      Week_1 = c(res_model_d1[[1]][1], res_model_d2[[1]][1], res_model_d3[[1]][1], res_model_d4[[1]][1]),
      Week_2 = c(res_model_d1[[1]][2], res_model_d2[[1]][2], res_model_d3[[1]][2], res_model_d4[[1]][2]),
      Week_3 = c(res_model_d1[[1]][3], res_model_d2[[1]][3], res_model_d3[[1]][3], res_model_d4[[1]][3]),
      Week_4 = c(res_model_d1[[1]][4], res_model_d2[[1]][4], res_model_d3[[1]][4], res_model_d4[[1]][4]),
      #create a total column and row for the sum of each week across all departments
      Total = c(sum(res_model_d1[[1]]), sum(res_model_d2[[1]]), sum(res_model_d3[[1]]), sum(res_model_d4[[1]]))
      
    )
    
    
    # Set the department names as row names
    # Round the values to one decimal point
    results[, `:=`(Week_1 = round(Week_1, 1),
                   Week_2 = round(Week_2, 1),
                   Week_3 = round(Week_3, 1),
                   Week_4 = round(Week_4, 1),
                   Total = round(Total, 1))]
    setDF(results, rownames = results$Department)
    
    # Set the column names for the four middle columns
    week_list <- as.character(month_data$Date)
    colnames(results)[2:5] <- week_list
    
    results$Department <- NULL
    
    # correct the code because it is printing the column total but no the new row with total    
    results <- rbind(results, colSums(results))
    rownames(results)[nrow(results)] <- "Total"
    
    
    output$pred_table <- renderDataTable({
      results
    }, options = list(dom = 't'))
    
    # # Create a plot to display the results data
    output$graph <- renderPlot({
      # Plotting without the last row (total)
      matplot(1:4, t(results[-nrow(results), -1]), type = "l", lty = 1, col = c("blue", "green", "red", "orange"), xlab = "Week", ylab = "Value", main = "Predictions Over Time", ylim = range(results[-nrow(results), -1]))
      
      # Adding lines for WSdep1, WSdep2, WSdep3, and WSdep4
      lines(1:4, month_data$WSdep1, col = "blue", lty = 2)
      lines(1:4, month_data$WSdep2, col = "green", lty = 2)
      lines(1:4, month_data$WSdep3, col = "red", lty = 2)
      lines(1:4, month_data$WSdep4, col = "orange", lty = 2)
      
      # Adding legend for all lines except the last one
      legend("topleft", legend = c(rownames(results)[-nrow(results)], "Real Dept 1", "Real Dept 2", "Real Dept 3", "Real Dept 4"), 
             lty = c(rep(1, 4), rep(2, 4)), 
             col = c(c("blue", "green", "red", "orange"), rep("black", 4)))
    })
    
    metrics <- data.table(
      Metrica = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
      rmae = c(res_model_d1[[2]], res_model_d2[[2]], res_model_d3[[2]], res_model_d4[[2]]),
      mae = c(res_model_d1[[3]], res_model_d2[[3]], res_model_d3[[3]], res_model_d4[[3]]),
      rmse = c(res_model_d1[[4]], res_model_d2[[4]], res_model_d3[[4]], res_model_d4[[4]]),
      r2 = c(res_model_d1[[5]], res_model_d2[[5]], res_model_d3[[5]], res_model_d4[[5]])
    )
    
    setDF(metrics, rownames = metrics$Metrica)
    
    metrics$Metrica <- NULL
    
    #Render table
    output$metrics_table <- renderDataTable({
      metrics
    }, options = list(dom = 't'))
    
    ####-------------------OPTIMIZATION-------------------####
    
    preds <- c(res_model_d1[[1]], res_model_d2[[1]], res_model_d3[[1]], res_model_d4[[1]])
    
    #Choose the optimization model to use, to pass it to function
    uni_opt_model_number <- switch(input$optmodel_uni,
                                   'MonteCarlo' =  1,
                                   'Sann' = 2,
                                   'Tabu' = 3,
                                   'HillClimb' = 4)
    
    
    
    #Choose the optimization objective to use, to pass it to function
    uni_opt_objective_model_number <- switch(input$obj_uni,
                                             'Objective 1 - Profit' =  1,
                                             'Objective 2 - Manual effort' = 2,
                                             'Both' = 3)
    
    #Call the model
    result_opt <- Optimization(as.numeric(uni_opt_model_number), as.numeric(uni_opt_objective_model_number), preds)
    
    print(result_opt)
    
    #Create another var to send to ui
    opt_name <- input$optmodel_uni
    
    #Send to ui
    output$opt_name <- renderText({
      paste("Model:", opt_name)
    })
    
    if (uni_opt_objective_model_number == 1 || uni_opt_objective_model_number == 2) {
      
      first_12_values <- result_opt$sol[1:12]
      
      # Create a matrix for the first 12 values with 3 rows and 4 columns
      matrix_first_12 <- matrix(first_12_values, nrow = 3, byrow = TRUE)
      
      
      opt_table_func <- data.table(
        Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
        Junior = matrix_first_12[, 1],
        Mid = matrix_first_12[, 2],
        Senior = matrix_first_12[, 3]
      )
      
      setDF(opt_table_func, rownames = opt_table_func$Department)
      
      opt_table_func$Department <- NULL
      
      # Render the data table
      output$opt_table_func <- renderDataTable({
        datatable(opt_table_func, options = list(dom = 't'), class = 'cell-border stripe',
                  rownames = TRUE) %>%
          formatStyle(names(opt_table_func), textAlign = 'center')
      })
      
      # Extracting the remaining 16 values
      remaining_16_values <- result_opt$sol[13:28]
      
      # Create a matrix for the remaining 16 values with 4 rows and 4 columns
      #matrix_remaining_16 <- matrix(c(remaining_16_values, rep(NA, 4)), nrow = 4, byrow = TRUE)
      
      matrix_remaining_16 <- matrix(remaining_16_values[1:16], nrow = 4, byrow = TRUE)
      
      print("adsasdadsadasd")
      print(matrix_remaining_16)
      
      # Create a data table for the remaining 16 values
      opt_table_opt <- data.table(
        Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
        Week_1 = matrix_remaining_16[, 1],
        Week_2 = matrix_remaining_16[, 2],
        Week_3 = matrix_remaining_16[, 3],
        Week_4 = matrix_remaining_16[, 4]
      )
      
      setDF(opt_table_opt, rownames = opt_table_opt$Department)
      month_data <- get_month_data(as.integer(month))
      week_list <- as.character(month_data$Date)
      colnames(opt_table_opt)[2:5] <- week_list
      opt_table_opt$Department <- NULL
      
      output$opt_table_opt <- renderDataTable({
        datatable(opt_table_opt, options = list(dom = 't'), class = 'cell-border stripe',
                  rownames = TRUE) %>%
          formatStyle(names(opt_table_opt), textAlign = 'center')
      })
      
      if (uni_opt_objective_model_number == 1) {
        output$obj1 <- renderText({
          paste("Objective 1 - Maximize the profit. The profit is:", result_opt$eval)
        })
      } else if (uni_opt_objective_model_number == 2) {
        output$obj2 <- renderText({
          paste("Objective 2 - Minimize the manual effort. The manual effort is:", result_opt$eval)
        })
      } 
      
      
      
    } else if (uni_opt_objective_model_number == 3) {
      
      #### --- Objective 1 --- ####
      
      if(identical(tolower(input$optmodel_uni), tolower("Tabu")) && identical(tolower(input$obj_uni), tolower("Both"))){
        
        print(result_opt)
        
        first_12_values <- result_opt[[1]]$sol[1:12]
        print(first_12_values)

        # Create a matrix for the first 12 values with 3 rows and 4 columns
        matrix_first_12 <- matrix(first_12_values, nrow = 3, byrow = TRUE)



        opt_table_func <- data.table(
          Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
          Junior = matrix_first_12[, 1],
          Mid = matrix_first_12[, 2],
          Senior = matrix_first_12[, 3]
        )

        setDF(opt_table_func, rownames = opt_table_func$Department)

        opt_table_func$Department <- NULL

        # Render the data table
        output$opt_table_func <- renderDataTable({
          datatable(opt_table_func, options = list(dom = 't'), class = 'cell-border stripe',
                    rownames = TRUE) %>%
            formatStyle(names(opt_table_func), textAlign = 'center')
        })

        # Extracting the remaining 16 values
        remaining_16_values <- result_opt[[1]]$sol[13:28]

        # Create a matrix for the remaining 16 values with 4 rows and 4 columns
        matrix_remaining_16 <- matrix(remaining_16_values[1:16], nrow = 4, byrow = TRUE)

        # Create a data table for the remaining 16 values
        opt_table_opt <- data.table(
          Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
          Week_1 = matrix_remaining_16[, 1],
          Week_2 = matrix_remaining_16[, 2],
          Week_3 = matrix_remaining_16[, 3],
          Week_4 = matrix_remaining_16[, 4]
        )

        setDF(opt_table_opt, rownames = opt_table_opt$Department)
        month_data <- get_month_data(as.integer(month))
        week_list <- as.character(month_data$Date)
        colnames(opt_table_opt)[2:5] <- week_list
        opt_table_opt$Department <- NULL

        output$opt_table_opt <- renderDataTable({
          datatable(opt_table_opt, options = list(dom = 't'), class = 'cell-border stripe',
                    rownames = TRUE) %>%
            formatStyle(names(opt_table_opt), textAlign = 'center')
        })


        #### --- Objective 2 --- ####
        first_12_values <- result_opt[[2]]$sol[1:12]
        #print(first_12_values)

        # Create a matrix for the first 12 values with 3 rows and 4 columns
        matrix_first_12 <- matrix(first_12_values, nrow = 3, byrow = TRUE)



        opt_table_func_2 <- data.table(
          Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
          Junior = matrix_first_12[, 1],
          Mid = matrix_first_12[, 2],
          Senior = matrix_first_12[, 3]
        )

        setDF(opt_table_func_2, rownames = opt_table_func_2$Department)

        opt_table_func_2$Department <- NULL

        # Render the data table
        output$opt_table_func_2 <- renderDataTable({
          datatable(opt_table_func_2, options = list(dom = 't'), class = 'cell-border stripe',
                    rownames = TRUE) %>%
            formatStyle(names(opt_table_func_2), textAlign = 'center')
        })

        # Extracting the remaining 16 values
        remaining_16_values <- result_opt[[2]]$sol[13:28]

        # Create a matrix for the remaining 16 values with 4 rows and 4 columns
        matrix_remaining_16 <- matrix(remaining_16_values[1:16], nrow = 4, byrow = TRUE)

        # Create a data table for the remaining 16 values
        opt_table_opt_2 <- data.table(
          Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
          Week_1 = matrix_remaining_16[, 1],
          Week_2 = matrix_remaining_16[, 2],
          Week_3 = matrix_remaining_16[, 3],
          Week_4 = matrix_remaining_16[, 4]
        )

        setDF(opt_table_opt_2, rownames = opt_table_opt_2$Department)
        month_data <- get_month_data(as.integer(month))
        week_list <- as.character(month_data$Date)
        colnames(opt_table_opt_2)[2:5] <- week_list
        opt_table_opt_2$Department <- NULL

        output$opt_table_opt_2 <- renderDataTable({
          datatable(opt_table_opt_2, options = list(dom = 't'), class = 'cell-border stripe',
                    rownames = TRUE) %>%
            formatStyle(names(opt_table_opt_2), textAlign = 'center')
        })

        output$obj3 <- renderText({
          paste("Multiobjetive - Minimize the manual effort and maximize the profit. The manual effort is:", result_opt[[2]]$monthly_effort, "and the profit is:", (result_opt[[1]]$monthly_profit)*(-1))
        })
        
      }else{
        #### ELSE
        first_12_values <- result_opt$obj_1$sol[1:12]
        print(first_12_values)
        
        # Create a matrix for the first 12 values with 3 rows and 4 columns
        matrix_first_12 <- matrix(first_12_values, nrow = 3, byrow = TRUE)
        
        
        
        opt_table_func <- data.table(
          Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
          Junior = matrix_first_12[, 1],
          Mid = matrix_first_12[, 2],
          Senior = matrix_first_12[, 3]
        )
        
        setDF(opt_table_func, rownames = opt_table_func$Department)
        
        opt_table_func$Department <- NULL
        
        # Render the data table
        output$opt_table_func <- renderDataTable({
          datatable(opt_table_func, options = list(dom = 't'), class = 'cell-border stripe',
                    rownames = TRUE) %>%
            formatStyle(names(opt_table_func), textAlign = 'center')
        })
        
        # Extracting the remaining 16 values
        remaining_16_values <- result_opt$obj_1$sol[13:28]   
        
        # Create a matrix for the remaining 16 values with 4 rows and 4 columns
        matrix_remaining_16 <- matrix(remaining_16_values[1:16], nrow = 4, byrow = TRUE)
        
        # Create a data table for the remaining 16 values
        opt_table_opt <- data.table(
          Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
          Week_1 = matrix_remaining_16[, 1],
          Week_2 = matrix_remaining_16[, 2],
          Week_3 = matrix_remaining_16[, 3],
          Week_4 = matrix_remaining_16[, 4]
        )
        
        setDF(opt_table_opt, rownames = opt_table_opt$Department)
        month_data <- get_month_data(as.integer(month))
        week_list <- as.character(month_data$Date)
        colnames(opt_table_opt)[2:5] <- week_list
        opt_table_opt$Department <- NULL
        
        output$opt_table_opt <- renderDataTable({
          datatable(opt_table_opt, options = list(dom = 't'), class = 'cell-border stripe',
                    rownames = TRUE) %>%
            formatStyle(names(opt_table_opt), textAlign = 'center')
        })   
        
        
        #### --- Objective 2 --- ####
        first_12_values <- result_opt$obj_2$sol[1:12]
        print(first_12_values)
        
        # Create a matrix for the first 12 values with 3 rows and 4 columns
        matrix_first_12 <- matrix(first_12_values, nrow = 3, byrow = TRUE)
        
        
        
        opt_table_func_2 <- data.table(
          Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
          Junior = matrix_first_12[, 1],
          Mid = matrix_first_12[, 2],
          Senior = matrix_first_12[, 3]
        )
        
        setDF(opt_table_func_2, rownames = opt_table_func_2$Department)
        
        opt_table_func_2$Department <- NULL
        
        # Render the data table
        output$opt_table_func_2 <- renderDataTable({
          datatable(opt_table_func_2, options = list(dom = 't'), class = 'cell-border stripe',
                    rownames = TRUE) %>%
            formatStyle(names(opt_table_func_2), textAlign = 'center')
        })
        
        # Extracting the remaining 16 values
        remaining_16_values <- result_opt$obj_2$sol[13:28]   
        
        # Create a matrix for the remaining 16 values with 4 rows and 4 columns
        matrix_remaining_16 <- matrix(remaining_16_values[1:16], nrow = 4, byrow = TRUE)
        
        # Create a data table for the remaining 16 values
        opt_table_opt_2 <- data.table(
          Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
          Week_1 = matrix_remaining_16[, 1],
          Week_2 = matrix_remaining_16[, 2],
          Week_3 = matrix_remaining_16[, 3],
          Week_4 = matrix_remaining_16[, 4]
        )
        
        setDF(opt_table_opt_2, rownames = opt_table_opt_2$Department)
        month_data <- get_month_data(as.integer(month))
        week_list <- as.character(month_data$Date)
        colnames(opt_table_opt_2)[2:5] <- week_list
        opt_table_opt_2$Department <- NULL
        
        output$opt_table_opt_2 <- renderDataTable({
          datatable(opt_table_opt_2, options = list(dom = 't'), class = 'cell-border stripe',
                    rownames = TRUE) %>%
            formatStyle(names(opt_table_opt_2), textAlign = 'center')
        })
        
        output$obj3 <- renderText({
          paste("Multiobjetive - Minimize the manual effort and maximize the profit. The manual effort is:", (result_opt$obj_2$eval)*(-1), "and the profit is:", result_opt$obj_1$eval)  
        })
      }
        #
      
      
      
      
    }
    
    
    ####-------------------END OPTIMIZATION-------------------####
    
    },ignoreInit = TRUE)
  
  ####-------------------END UNIVARIATE-------------------#### 
  
  ####-------------------MULTIVARIATE-------------------####  
  
  observeEvent(input$predict_btn_multi,{
    
    multi_model = input$multimodel_multi
    month = as.integer(input$week_uni)
    
    #Create another var to send to ui
    used_model <- input$multimodel_multi
    
    #Send to ui
    output$model_name <- renderText({
      paste("Model:", used_model)
    })
    
    output$metrics <- renderText({
      paste("Metrics")
    })
    
    multi_model_number <- switch(multi_model,
                               'ARIMAX' =  1,
                               'MultiHW' = 2,
                               'VAR' = 3)
    
    res_model <- MultivariateModel(as.integer(multi_model_number), as.integer(month))
    
    month_data <- get_month_data(as.integer(month))
    
    # Create a data frame with the results
    # Extracting values from the first part of res_model
    week_1_values <- res_model[[1]][, 1]
    week_2_values <- res_model[[1]][, 2]
    week_3_values <- res_model[[1]][, 3]
    week_4_values <- res_model[[1]][, 4]
    
    # Creating the data table
    pred_table <- data.table(
      Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
      Week_1 = week_1_values,
      Week_2 = week_2_values,
      Week_3 = week_3_values,
      Week_4 = week_4_values,
      Total = c(sum(res_model[[1]]))
    )
    
    
    # Set the department names as row names
    # Round the values to one decimal point
    pred_table[, `:=`(Week_1 = round(Week_1, 1),
                       Week_2 = round(Week_2, 1),
                       Week_3 = round(Week_3, 1),
                       Week_4 = round(Week_4, 1),
                       Total = round(Total, 1))]
    setDF(pred_table, rownames = pred_table$Department)
    
    # Set the column names for the four middle columns
    week_list <- as.character(month_data$Date)
    colnames(pred_table)[2:5] <- week_list
    
    pred_table$Department <- NULL
    
    # correct the code because it is printing the column total but no the new row with total    
    pred_table <- rbind(pred_table, colSums(pred_table))
    rownames(pred_table)[nrow(pred_table)] <- "Total"
    
    
    output$pred_table <- renderDataTable({
      datatable(pred_table, options = list(dom = 't'), class = 'cell-border stripe',
                rownames = TRUE) %>%
        formatStyle(names(pred_table), textAlign = 'center')
    })
    
    
    # Correcting the graph plotting code
    output$graph <- renderPlot({
      # Convert table_multi back to matrix format for plotting
      matrix_data <- as.matrix(pred_table[-nrow(pred_table), -1]) # Excluding the Total row and Department column
      
      # Plotting without the last row (total)
      matplot(1:4, t(matrix_data), type = "l", lty = 1, col = c("blue", "green", "red", "orange"), 
              xlab = "Week", ylab = "Value", main = "Predictions Over Time", ylim = range(matrix_data))
      
      # Adding lines for WSdep1, WSdep2, WSdep3, and WSdep4
      lines(1:4, month_data$WSdep1, col = "blue", lty = 2)
      lines(1:4, month_data$WSdep2, col = "green", lty = 2)
      lines(1:4, month_data$WSdep3, col = "red", lty = 2)
      lines(1:4, month_data$WSdep4, col = "orange", lty = 2)
      
      # Adding legend for all lines
      legend("topleft", legend = c(rownames(pred_table)[-nrow(pred_table)], "Real Dept 1", "Real Dept 2", "Real Dept 3", "Real Dept 4"), 
             lty = c(rep(1, 4), rep(2, 4)), 
             col = c(c("blue", "green", "red", "orange"), rep("black", 4)))
    })
    
    # Assuming metrics data creation and rendering is also included here
    metrics_table_multi <- data.table(
      Metrica = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
      rmae = c(res_model[[2]]$nmae),
      mae = c(res_model[[2]]$mae),
      rmse = c(res_model[[2]]$rmse),
      r2 = c(res_model[[2]]$r2)
    )
    setDF(metrics_table_multi, rownames = metrics_table_multi$Metrica)
    metrics_table_multi$Metrica <- NULL
    
    output$metrics_table <- renderDataTable({
      datatable(metrics_table_multi, options = list(dom = 't'), class = 'cell-border stripe',
                rownames = TRUE) %>%
        formatStyle(names(metrics_table_multi), textAlign = 'center')
    })
    
    
    #####-------------------OPTIMIZATION-------------------####
    
    preds <- res_model[[1]]
    
    #Choose the optimization model to use, to pass it to function
    multi_opt_model_number <- switch(input$optmodel_multi,
                                   'MonteCarlo' =  1,
                                   'Sann' = 2,
                                   'Tabu' = 3,
                                   'HillClimb' = 4)
    
    
    
    #Choose the optimization objective to use, to pass it to function
    multi_opt_objective_model_number <- switch(input$obj_multi,
                                             'Objective 1 - Profit' =  1,
                                             'Objective 2 - Manual effort' = 2,
                                             'Both' = 3)
    #Call the model
    result_opt <- Optimization(as.numeric(multi_opt_model_number), as.numeric(multi_opt_objective_model_number), preds)
    
    #Create another var to send to ui
    opt_name <- input$optmodel_uni
    
    #Send to ui
    output$opt_name <- renderText({
      paste("Model:", opt_name)
    })
    
    if (multi_opt_objective_model_number == 1 || multi_opt_objective_model_number == 2) {
      
      first_12_values <- result_opt$sol[1:12]
      
      # Create a matrix for the first 12 values with 3 rows and 4 columns
      matrix_first_12 <- matrix(first_12_values, nrow = 3, byrow = TRUE)
      
      
      opt_table_func <- data.table(
        Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
        Junior = matrix_first_12[, 1],
        Mid = matrix_first_12[, 2],
        Senior = matrix_first_12[, 3]
      )
      
      setDF(opt_table_func, rownames = opt_table_func$Department)
      
      opt_table_func$Department <- NULL
      
      # Render the data table
      output$opt_table_func <- renderDataTable({
        datatable(opt_table_func, options = list(dom = 't'), class = 'cell-border stripe',
                  rownames = TRUE) %>%
          formatStyle(names(opt_table_func), textAlign = 'center')
      })
      
      # Extracting the remaining 16 values
      remaining_16_values <- result_opt$sol[13:28]
      
      # Create a matrix for the remaining 16 values with 4 rows and 4 columns
      #matrix_remaining_16 <- matrix(c(remaining_16_values, rep(NA, 4)), nrow = 4, byrow = TRUE)
      
      matrix_remaining_16 <- matrix(remaining_16_values[1:16], nrow = 4, byrow = TRUE)
      
      print("adsasdadsadasd")
      print(matrix_remaining_16)
      
      # Create a data table for the remaining 16 values
      opt_table_opt <- data.table(
        Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
        Week_1 = matrix_remaining_16[, 1],
        Week_2 = matrix_remaining_16[, 2],
        Week_3 = matrix_remaining_16[, 3],
        Week_4 = matrix_remaining_16[, 4]
      )
      
      setDF(opt_table_opt, rownames = opt_table_opt$Department)
      month_data <- get_month_data(as.integer(month))
      week_list <- as.character(month_data$Date)
      colnames(opt_table_opt)[2:5] <- week_list
      opt_table_opt$Department <- NULL
      
      output$opt_table_opt <- renderDataTable({
        datatable(opt_table_opt, options = list(dom = 't'), class = 'cell-border stripe',
                  rownames = TRUE) %>%
          formatStyle(names(opt_table_opt), textAlign = 'center')
      })
      
      if (multi_opt_objective_model_number == 1) {
        output$obj1 <- renderText({
          paste("Objective 1 - Maximize the profit. The profit is:", result_opt$eval)
        })
      } else if (multi_opt_objective_model_number == 2) {
        output$obj2 <- renderText({
          paste("Objective 2 - Minimize the manual effort. The manual effort is:", result_opt$eval)
        })
      } 
      
      
      
    } else if (multi_opt_objective_model_number == 3) {
      
      #### --- Objective 1 --- ####
      
      if(identical(tolower(input$optmodel_multi), tolower("Tabu")) && identical(tolower(input$obj_multi), tolower("Both"))){
        
        print(result_opt)
        
        first_12_values <- result_opt[[1]]$sol[1:12]
        print(first_12_values)
        
        # Create a matrix for the first 12 values with 3 rows and 4 columns
        matrix_first_12 <- matrix(first_12_values, nrow = 3, byrow = TRUE)
        
        
        
        opt_table_func <- data.table(
          Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
          Junior = matrix_first_12[, 1],
          Mid = matrix_first_12[, 2],
          Senior = matrix_first_12[, 3]
        )
        
        setDF(opt_table_func, rownames = opt_table_func$Department)
        
        opt_table_func$Department <- NULL
        
        # Render the data table
        output$opt_table_func <- renderDataTable({
          datatable(opt_table_func, options = list(dom = 't'), class = 'cell-border stripe',
                    rownames = TRUE) %>%
            formatStyle(names(opt_table_func), textAlign = 'center')
        })
        
        # Extracting the remaining 16 values
        remaining_16_values <- result_opt[[1]]$sol[13:28]
        
        # Create a matrix for the remaining 16 values with 4 rows and 4 columns
        matrix_remaining_16 <- matrix(remaining_16_values[1:16], nrow = 4, byrow = TRUE)
        
        # Create a data table for the remaining 16 values
        opt_table_opt <- data.table(
          Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
          Week_1 = matrix_remaining_16[, 1],
          Week_2 = matrix_remaining_16[, 2],
          Week_3 = matrix_remaining_16[, 3],
          Week_4 = matrix_remaining_16[, 4]
        )
        
        setDF(opt_table_opt, rownames = opt_table_opt$Department)
        month_data <- get_month_data(as.integer(month))
        week_list <- as.character(month_data$Date)
        colnames(opt_table_opt)[2:5] <- week_list
        opt_table_opt$Department <- NULL
        
        output$opt_table_opt <- renderDataTable({
          datatable(opt_table_opt, options = list(dom = 't'), class = 'cell-border stripe',
                    rownames = TRUE) %>%
            formatStyle(names(opt_table_opt), textAlign = 'center')
        })
        
        
        #### --- Objective 2 --- ####
        first_12_values <- result_opt[[2]]$sol[1:12]
        #print(first_12_values)
        
        # Create a matrix for the first 12 values with 3 rows and 4 columns
        matrix_first_12 <- matrix(first_12_values, nrow = 3, byrow = TRUE)
        
        
        
        opt_table_func_2 <- data.table(
          Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
          Junior = matrix_first_12[, 1],
          Mid = matrix_first_12[, 2],
          Senior = matrix_first_12[, 3]
        )
        
        setDF(opt_table_func_2, rownames = opt_table_func_2$Department)
        
        opt_table_func_2$Department <- NULL
        
        # Render the data table
        output$opt_table_func_2 <- renderDataTable({
          datatable(opt_table_func_2, options = list(dom = 't'), class = 'cell-border stripe',
                    rownames = TRUE) %>%
            formatStyle(names(opt_table_func_2), textAlign = 'center')
        })
        
        # Extracting the remaining 16 values
        remaining_16_values <- result_opt[[2]]$sol[13:28]
        
        # Create a matrix for the remaining 16 values with 4 rows and 4 columns
        matrix_remaining_16 <- matrix(remaining_16_values[1:16], nrow = 4, byrow = TRUE)
        
        # Create a data table for the remaining 16 values
        opt_table_opt_2 <- data.table(
          Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
          Week_1 = matrix_remaining_16[, 1],
          Week_2 = matrix_remaining_16[, 2],
          Week_3 = matrix_remaining_16[, 3],
          Week_4 = matrix_remaining_16[, 4]
        )
        
        setDF(opt_table_opt_2, rownames = opt_table_opt_2$Department)
        month_data <- get_month_data(as.integer(month))
        week_list <- as.character(month_data$Date)
        colnames(opt_table_opt_2)[2:5] <- week_list
        opt_table_opt_2$Department <- NULL
        
        output$opt_table_opt_2 <- renderDataTable({
          datatable(opt_table_opt_2, options = list(dom = 't'), class = 'cell-border stripe',
                    rownames = TRUE) %>%
            formatStyle(names(opt_table_opt_2), textAlign = 'center')
        })
        
        output$obj3 <- renderText({
          paste("Multiobjetive - Minimize the manual effort and maximize the profit. The manual effort is:", result_opt[[2]]$monthly_effort, "and the profit is:", (result_opt[[1]]$monthly_profit)*(-1))
        })
        
      }else{
        #### ELSE
        first_12_values <- result_opt$obj_1$sol[1:12]
        print(first_12_values)
        
        # Create a matrix for the first 12 values with 3 rows and 4 columns
        matrix_first_12 <- matrix(first_12_values, nrow = 3, byrow = TRUE)
        
        
        
        opt_table_func <- data.table(
          Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
          Junior = matrix_first_12[, 1],
          Mid = matrix_first_12[, 2],
          Senior = matrix_first_12[, 3]
        )
        
        setDF(opt_table_func, rownames = opt_table_func$Department)
        
        opt_table_func$Department <- NULL
        
        # Render the data table
        output$opt_table_func <- renderDataTable({
          datatable(opt_table_func, options = list(dom = 't'), class = 'cell-border stripe',
                    rownames = TRUE) %>%
            formatStyle(names(opt_table_func), textAlign = 'center')
        })
        
        # Extracting the remaining 16 values
        remaining_16_values <- result_opt$obj_1$sol[13:28]   
        
        # Create a matrix for the remaining 16 values with 4 rows and 4 columns
        matrix_remaining_16 <- matrix(remaining_16_values[1:16], nrow = 4, byrow = TRUE)
        
        # Create a data table for the remaining 16 values
        opt_table_opt <- data.table(
          Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
          Week_1 = matrix_remaining_16[, 1],
          Week_2 = matrix_remaining_16[, 2],
          Week_3 = matrix_remaining_16[, 3],
          Week_4 = matrix_remaining_16[, 4]
        )
        
        setDF(opt_table_opt, rownames = opt_table_opt$Department)
        month_data <- get_month_data(as.integer(month))
        week_list <- as.character(month_data$Date)
        colnames(opt_table_opt)[2:5] <- week_list
        opt_table_opt$Department <- NULL
        
        output$opt_table_opt <- renderDataTable({
          datatable(opt_table_opt, options = list(dom = 't'), class = 'cell-border stripe',
                    rownames = TRUE) %>%
            formatStyle(names(opt_table_opt), textAlign = 'center')
        })   
        
        
        #### --- Objective 2 --- ####
        first_12_values <- result_opt$obj_2$sol[1:12]
        print(first_12_values)
        
        # Create a matrix for the first 12 values with 3 rows and 4 columns
        matrix_first_12 <- matrix(first_12_values, nrow = 3, byrow = TRUE)
        
        
        
        opt_table_func_2 <- data.table(
          Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
          Junior = matrix_first_12[, 1],
          Mid = matrix_first_12[, 2],
          Senior = matrix_first_12[, 3]
        )
        
        setDF(opt_table_func_2, rownames = opt_table_func_2$Department)
        
        opt_table_func_2$Department <- NULL
        
        # Render the data table
        output$opt_table_func_2 <- renderDataTable({
          datatable(opt_table_func_2, options = list(dom = 't'), class = 'cell-border stripe',
                    rownames = TRUE) %>%
            formatStyle(names(opt_table_func_2), textAlign = 'center')
        })
        
        # Extracting the remaining 16 values
        remaining_16_values <- result_opt$obj_2$sol[13:28]   
        
        # Create a matrix for the remaining 16 values with 4 rows and 4 columns
        matrix_remaining_16 <- matrix(remaining_16_values[1:16], nrow = 4, byrow = TRUE)
        
        # Create a data table for the remaining 16 values
        opt_table_opt_2 <- data.table(
          Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
          Week_1 = matrix_remaining_16[, 1],
          Week_2 = matrix_remaining_16[, 2],
          Week_3 = matrix_remaining_16[, 3],
          Week_4 = matrix_remaining_16[, 4]
        )
        
        setDF(opt_table_opt_2, rownames = opt_table_opt_2$Department)
        month_data <- get_month_data(as.integer(month))
        week_list <- as.character(month_data$Date)
        colnames(opt_table_opt_2)[2:5] <- week_list
        opt_table_opt_2$Department <- NULL
        
        output$opt_table_opt_2 <- renderDataTable({
          datatable(opt_table_opt_2, options = list(dom = 't'), class = 'cell-border stripe',
                    rownames = TRUE) %>%
            formatStyle(names(opt_table_opt_2), textAlign = 'center')
        })
        
        output$obj3 <- renderText({
          paste("Multiobjetive - Minimize the manual effort and maximize the profit. The manual effort is:", (result_opt$obj_2$eval)*(-1), "and the profit is:", result_opt$obj_1$eval)  
        })
      }
      
      
    }
    
    #####-------------------END OPTIMIZATION-------------------####
    
  },ignoreInit = TRUE)
  
  ####-------------------END MULTIVARIATE-------------------####  
  
  ####-------------------BEST MODEL-------------------####
  
  observeEvent(input$predict_btn_best,{
    #Get the months that the user wants to predict
    month = as.integer(input$week_uni)
    
    #Send to ui
    output$model_name <- renderText({
      paste("Model:", "Random Forest")
    })
    
    #Make Metrics only appear when button is clicked
    output$metrics <- renderText({
      paste("Metrics")
    })
    
    #Call Random forest for all 4 departments
    res_model_d1 <- UnivariateModel(8, as.double(month), 1)
    res_model_d2 <- UnivariateModel(8, as.double(month), 2)
    res_model_d3 <- UnivariateModel(8, as.double(month), 3)
    res_model_d4 <- UnivariateModel(8, as.double(month), 4)
    
    month_data <- get_month_data(as.integer(month))
    
    # Create a data frame with the results
    results <- data.table(
      Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
      Week_1 = c(res_model_d1[[1]][1], res_model_d2[[1]][1], res_model_d3[[1]][1], res_model_d4[[1]][1]),
      Week_2 = c(res_model_d1[[1]][2], res_model_d2[[1]][2], res_model_d3[[1]][2], res_model_d4[[1]][2]),
      Week_3 = c(res_model_d1[[1]][3], res_model_d2[[1]][3], res_model_d3[[1]][3], res_model_d4[[1]][3]),
      Week_4 = c(res_model_d1[[1]][4], res_model_d2[[1]][4], res_model_d3[[1]][4], res_model_d4[[1]][4]),
      #create a total column and row for the sum of each week across all departments
      Total = c(sum(res_model_d1[[1]]), sum(res_model_d2[[1]]), sum(res_model_d3[[1]]), sum(res_model_d4[[1]]))
    )
    
    
    # Set the department names as row names
    # Round the values to one decimal point
    results[, `:=`(Week_1 = round(Week_1, 1),
                   Week_2 = round(Week_2, 1),
                   Week_3 = round(Week_3, 1),
                   Week_4 = round(Week_4, 1),
                   Total = round(Total, 1))]
    setDF(results, rownames = results$Department)
    
    # Set the column names for the four middle columns
    week_list <- as.character(month_data$Date)
    colnames(results)[2:5] <- week_list
    
    results$Department <- NULL
    
    # correct the code because it is printing the column total but no the new row with total    
    results <- rbind(results, colSums(results))
    rownames(results)[nrow(results)] <- "Total"
    
    
    output$pred_table <- renderDataTable({
      results
    }, options = list(dom = 't'))
    
    # # Create a plot to display the results data
    output$graph <- renderPlot({
      # Plotting without the last row (total)
      matplot(1:4, t(results[-nrow(results), -1]), type = "l", lty = 1, col = c("blue", "green", "red", "orange"), xlab = "Week", ylab = "Value", main = "Predictions Over Time", ylim = range(results[-nrow(results), -1]))
      
      # Adding lines for WSdep1, WSdep2, WSdep3, and WSdep4
      lines(1:4, month_data$WSdep1, col = "blue", lty = 2)
      lines(1:4, month_data$WSdep2, col = "green", lty = 2)
      lines(1:4, month_data$WSdep3, col = "red", lty = 2)
      lines(1:4, month_data$WSdep4, col = "orange", lty = 2)
      
      # Adding legend for all lines except the last one
      legend("topleft", legend = c(rownames(results)[-nrow(results)], "Real Dept 1", "Real Dept 2", "Real Dept 3", "Real Dept 4"), 
             lty = c(rep(1, 4), rep(2, 4)), 
             col = c(c("blue", "green", "red", "orange"), rep("black", 4)))
    })
    
    # Put the table for metrics
    metrics <- data.table(
      Metrica = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
      rmae = c(res_model_d1[[2]], res_model_d2[[2]], res_model_d3[[2]], res_model_d4[[2]]),
      mae = c(res_model_d1[[3]], res_model_d2[[3]], res_model_d3[[3]], res_model_d4[[3]]),
      rmse = c(res_model_d1[[4]], res_model_d2[[4]], res_model_d3[[4]], res_model_d4[[4]]),
      r2 = c(res_model_d1[[5]], res_model_d2[[5]], res_model_d3[[5]], res_model_d4[[5]])
    )
    
    setDF(metrics, rownames = metrics$Metrica)
    
    metrics$Metrica <- NULL
    
    #Render table for metrics
    output$metrics_table <- renderDataTable({
      metrics
    }, options = list(dom = 't'))
    
    #####-------------------OPTIMIZATION-------------------####
    
    #Make preds variable to pass to optimization
    preds <- c(res_model_d1[[1]], res_model_d2[[1]], res_model_d3[[1]], res_model_d4[[1]])
  
    #Choose the optimization objective to use, to pass it to function
    best_opt_objective_model_number <- switch(input$obj_best,
                                             'Objective 1 - Profit' =  1,
                                             'Objective 2 - Manual effort' = 2,
                                             'Both' = 3)
    
    #Call the model
    result_opt <- Optimization(1, best_opt_objective_model_number, preds)
  
    #Send to ui
    output$opt_name <- renderText({
      paste("Model:", "TabuSearch")
    })
    
    if (best_opt_objective_model_number == 1 || best_opt_objective_model_number == 2) {
      
      first_12_values <- result_opt$sol[1:12]
      
      # Create a matrix for the first 12 values with 3 rows and 4 columns
      matrix_first_12 <- matrix(first_12_values, nrow = 3, byrow = TRUE)
      
      
      opt_table_func <- data.table(
        Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
        Junior = matrix_first_12[, 1],
        Mid = matrix_first_12[, 2],
        Senior = matrix_first_12[, 3]
      )
      
      setDF(opt_table_func, rownames = opt_table_func$Department)
      
      opt_table_func$Department <- NULL
      
      # Render the data table
      output$opt_table_func <- renderDataTable({
        datatable(opt_table_func, options = list(dom = 't'), class = 'cell-border stripe',
                  rownames = TRUE) %>%
          formatStyle(names(opt_table_func), textAlign = 'center')
      })
      
      # Extracting the remaining 16 values
      remaining_16_values <- result_opt$sol[13:28]
      
      # Create a matrix for the remaining 16 values with 4 rows and 4 columns
      #matrix_remaining_16 <- matrix(c(remaining_16_values, rep(NA, 4)), nrow = 4, byrow = TRUE)
      
      matrix_remaining_16 <- matrix(remaining_16_values[1:16], nrow = 4, byrow = TRUE)
      
      print("adsasdadsadasd")
      print(matrix_remaining_16)
      
      # Create a data table for the remaining 16 values
      opt_table_opt <- data.table(
        Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
        Week_1 = matrix_remaining_16[, 1],
        Week_2 = matrix_remaining_16[, 2],
        Week_3 = matrix_remaining_16[, 3],
        Week_4 = matrix_remaining_16[, 4]
      )
      
      setDF(opt_table_opt, rownames = opt_table_opt$Department)
      month_data <- get_month_data(as.integer(month))
      week_list <- as.character(month_data$Date)
      colnames(opt_table_opt)[2:5] <- week_list
      opt_table_opt$Department <- NULL
      
      output$opt_table_opt <- renderDataTable({
        datatable(opt_table_opt, options = list(dom = 't'), class = 'cell-border stripe',
                  rownames = TRUE) %>%
          formatStyle(names(opt_table_opt), textAlign = 'center')
      })
      
      if (best_opt_objective_model_number == 1) {
        output$obj1 <- renderText({
          paste("Objective 1 - Maximize the profit. The profit is:", result_opt$eval)
        })
      } else if (best_opt_objective_model_number == 2) {
        output$obj2 <- renderText({
          paste("Objective 2 - Minimize the manual effort. The manual effort is:", result_opt$eval)
        })
      } 
      
      
      
    } else if (best_opt_objective_model_number == 3) {
      
      #### --- Objective 1 --- ####
      
      first_12_values <- result_opt$obj_1$sol[1:12]
      print(first_12_values)
      
      # Create a matrix for the first 12 values with 3 rows and 4 columns
      matrix_first_12 <- matrix(first_12_values, nrow = 3, byrow = TRUE)
      
      
      
      opt_table_func <- data.table(
        Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
        Junior = matrix_first_12[, 1],
        Mid = matrix_first_12[, 2],
        Senior = matrix_first_12[, 3]
      )
      
      setDF(opt_table_func, rownames = opt_table_func$Department)
      
      opt_table_func$Department <- NULL
      
      # Render the data table
      output$opt_table_func <- renderDataTable({
        datatable(opt_table_func, options = list(dom = 't'), class = 'cell-border stripe',
                  rownames = TRUE) %>%
          formatStyle(names(opt_table_func), textAlign = 'center')
      })
      
      # Extracting the remaining 16 values
      remaining_16_values <- result_opt$obj_1$sol[13:28]   
      
      # Create a matrix for the remaining 16 values with 4 rows and 4 columns
      matrix_remaining_16 <- matrix(remaining_16_values[1:16], nrow = 4, byrow = TRUE)
      
      # Create a data table for the remaining 16 values
      opt_table_opt <- data.table(
        Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
        Week_1 = matrix_remaining_16[, 1],
        Week_2 = matrix_remaining_16[, 2],
        Week_3 = matrix_remaining_16[, 3],
        Week_4 = matrix_remaining_16[, 4]
      )
      
      setDF(opt_table_opt, rownames = opt_table_opt$Department)
      month_data <- get_month_data(as.integer(month))
      week_list <- as.character(month_data$Date)
      colnames(opt_table_opt)[2:5] <- week_list
      opt_table_opt$Department <- NULL
      
      output$opt_table_opt <- renderDataTable({
        datatable(opt_table_opt, options = list(dom = 't'), class = 'cell-border stripe',
                  rownames = TRUE) %>%
          formatStyle(names(opt_table_opt), textAlign = 'center')
      })   
      
      
      #### --- Objective 2 --- ####
      first_12_values <- result_opt$obj_2$sol[1:12]
      print(first_12_values)
      
      # Create a matrix for the first 12 values with 3 rows and 4 columns
      matrix_first_12 <- matrix(first_12_values, nrow = 3, byrow = TRUE)
      
      
      
      opt_table_func_2 <- data.table(
        Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
        Junior = matrix_first_12[, 1],
        Mid = matrix_first_12[, 2],
        Senior = matrix_first_12[, 3]
      )
      
      setDF(opt_table_func_2, rownames = opt_table_func_2$Department)
      
      opt_table_func_2$Department <- NULL
      
      # Render the data table
      output$opt_table_func_2 <- renderDataTable({
        datatable(opt_table_func_2, options = list(dom = 't'), class = 'cell-border stripe',
                  rownames = TRUE) %>%
          formatStyle(names(opt_table_func_2), textAlign = 'center')
      })
      
      # Extracting the remaining 16 values
      remaining_16_values <- result_opt$obj_2$sol[13:28]   
      
      # Create a matrix for the remaining 16 values with 4 rows and 4 columns
      matrix_remaining_16 <- matrix(remaining_16_values[1:16], nrow = 4, byrow = TRUE)
      
      # Create a data table for the remaining 16 values
      opt_table_opt_2 <- data.table(
        Department = c("Departamento 1", "Departamento 2", "Departamento 3", "Departamento 4"),
        Week_1 = matrix_remaining_16[, 1],
        Week_2 = matrix_remaining_16[, 2],
        Week_3 = matrix_remaining_16[, 3],
        Week_4 = matrix_remaining_16[, 4]
      )
      
      setDF(opt_table_opt_2, rownames = opt_table_opt_2$Department)
      month_data <- get_month_data(as.integer(month))
      week_list <- as.character(month_data$Date)
      colnames(opt_table_opt_2)[2:5] <- week_list
      opt_table_opt_2$Department <- NULL
      
      output$opt_table_opt_2 <- renderDataTable({
        datatable(opt_table_opt_2, options = list(dom = 't'), class = 'cell-border stripe',
                  rownames = TRUE) %>%
          formatStyle(names(opt_table_opt_2), textAlign = 'center')
      })
      
      output$obj3 <- renderText({
        paste("Multiobjetive - Minimize the manual effort and maximize the profit. The manual effort is:", result_opt$obj_2$eval, "and the profit is:", (result_opt$obj_1$eval)*(-1))  
      })
      
      
    }
    
    #####-------------------END OPTIMIZATION-------------------####
    },ignoreInit = TRUE)
  
  ####-------------------END BEST MODEL-------------------####
}

# Run the application
shinyApp(ui = ui, server = server)
