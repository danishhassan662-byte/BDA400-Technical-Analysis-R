ema <- function(data, period) {
  
  if (period <= 0) {
    stop("Period must be greater than zero")
  }
  
  if (length(data) == 0) {
    stop("Data cannot be empty")
  }
  
  result <- rep(NA_real_, length(data))
  
  multiplier <- 2 / (period + 1)
  
  result[1] <- data[1]
  
  for (i in 2:length(data)) {
    result[i] <- (data[i] * multiplier) +
      (result[i - 1] * (1 - multiplier))
  }
  
  return(result)
}
