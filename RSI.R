
rsi <- function(data, period) {
  
  changes <- diff(data)
  
  gains <- pmax(changes, 0)
  losses <- pmax(-changes, 0)
  
  avg_gain <- rep(NA_real_, length(data))
  avg_loss <- rep(NA_real_, length(data))
  result <- rep(NA_real_, length(data))
  
  if (length(data) <= period) {
    return(result)
  }
  
  avg_gain[period + 1] <- mean(gains[1:period])
  avg_loss[period + 1] <- mean(losses[1:period])
  
  for (i in (period + 2):length(data)) {
    avg_gain[i] <- ((avg_gain[i - 1] * (period - 1)) +
                      gains[i - 1]) / period
    
    avg_loss[i] <- ((avg_loss[i - 1] * (period - 1)) +
                      losses[i - 1]) / period
  }
  
  for (i in (period + 1):length(data)) {
    if (avg_loss[i] == 0) {
      result[i] <- 100
    } else {
      rs <- avg_gain[i] / avg_loss[i]
      result[i] <- 100 - (100 / (1 + rs))
    }
  }
  
  return(result)
}
