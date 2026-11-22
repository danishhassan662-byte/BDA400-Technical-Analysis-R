ema <- function(data, period) {
  
  result <- rep(NA_real_, length(data))
  
  valid <- which(!is.na(data))
  
  if (length(valid) < period) {
    return(result)
  }
  
  start <- valid[period]
  
  result[start] <- mean(data[valid[1:period]])
  
  multiplier <- 2 / (period + 1)
  
  for (i in (start + 1):length(data)) {
    if (!is.na(data[i])) {
      result[i] <- data[i] * multiplier +
        result[i - 1] * (1 - multiplier)
    }
  }
  
  return(result)
}