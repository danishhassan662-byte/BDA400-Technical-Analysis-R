sma <- function(data, period) {
  result <- rep(NA, length(data))
  
  for (i in period:length(data)) {
    result[i] <- mean(data[(i - period + 1):i])
  }
  
  return(result)
}

prices <- c(10, 20, 30, 40, 50, 60)

sma(prices, 3)