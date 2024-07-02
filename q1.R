#function to perform simple and multiple linear regression
mult_reg <- function(Y, X) {
  #add intercept to X matrix
  X <- cbind(1, X)
  
  #calculate regression coefficients
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  
  #estimated Y values
  Y_hat <- X %*% beta
  
  #residuals
  residuals <- Y - Y_hat
  
  #total sum of squares
  TSS <- sum((Y - mean(Y))^2)
  
  #residual sum of squares
  RSS <- sum(residuals^2)
  
  #regression model sum of squares
  RMSS <- TSS - RSS
  
  #coefficient of determination (R-squared)
  R_squared <- RMSS / TSS
  
  #output regression coefficients, estimates of Y,residuals, TSS, RMSS, RSS, and R-squared
  output <- list("Coefficients" = beta,
                 "Estimated_Y" = Y_hat,
                 "Residuals" = residuals,
                 "TSS" = TSS,
                 "RMSS" = RMSS,
                 "RSS" = RSS,
                 "R_squared" = R_squared)
  
  return(output)
}


#predict Y using X3 and X4:
result <- mult_reg(data$Y, cbind(data$X3, data$X4))
print(result)

