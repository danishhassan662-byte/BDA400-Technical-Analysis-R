linreg <- function(data, period) {
  
  result <- rep(NA_real_, length(data))
  
  for (i in period:length(data)) {
    
    window <- data[(i - period + 1):i]
    
    x <- 1:period
    
    model <- lm(window ~ x)
    
    result[i] <- predict(
      model,
      newdata = data.frame(x = period)
    )
  }
  
  return(result)
}

prices <- c(10, 20, 30, 40, 50, 60)

linreg(prices, 3)