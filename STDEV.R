stdev <- function(data, period) {
  
  result <- rep(NA_real_, length(data))
  
  for (i in period:length(data)) {
    window <- data[(i - period + 1):i]
    result[i] <- sd(window)
  }
  
  return(result)
}

prices <- c(10, 20, 30, 40, 50, 60)

stdev(prices, 3)