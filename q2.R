#function to do model selection based on R-square
model_selection <- function(data) {
  #number of independent variables excluding the dependent variable
  num_vars <- ncol(data) - 1
  
  #initialize a list to store model information
  model_list <- list()
  model_counter <- 1  
  
  #loop through all possible combinations of independent variables
  for (i in 1:num_vars) {
    combos <- combn(names(data)[-1], i)  
    
    #loop through each combination
    for (j in 1:ncol(combos)) {
      #select variables for the current combination
      selected_vars <- combos[, j]
      
      #fit the regression model
      formula <- as.formula(paste("Y ~", paste(selected_vars, collapse = " + ")))
      model <- lm(formula, data = data)
      
      #calculate model performance metrics
      Y_hat <- predict(model)
      TSS <- sum((data$Y - mean(data$Y))^2)
      RMSS <- sum((Y_hat - mean(data$Y))^2)
      RSS <- sum((data$Y - Y_hat)^2)
      R_Square <- summary(model)$r.squared
      
      #append model information to the list
      model_list <- c(model_list, list(data.frame(Model = model_counter,
                                                  Num_Vars = i,
                                                  Variables = paste(selected_vars, collapse = " "),
                                                  TSS = TSS,
                                                  RMSS = RMSS,
                                                  RSS = RSS,
                                                  R_Square = R_Square)))
      model_counter <- model_counter + 1  # Increment model counter
    }
  }
  
  #combine all dataframes into one dataframe
  model_df <- do.call(rbind, model_list)
  
  #sort models by R-squared in descending order within each number of variables
  model_df <- model_df[order(-model_df$R_Square, -model_df$Num_Vars), ]
  
  #reset row names
  rownames(model_df) <- NULL
  
  return(model_df)
}
result <- model_selection(data)
print(result)
