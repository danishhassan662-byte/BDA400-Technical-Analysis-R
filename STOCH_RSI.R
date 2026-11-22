
source("RSI.R")

stoch_rsi <- function(data, period, k_period, d_period) {
  
  rsi_values <- rsi(data, period)
  
  normalized <- rep(NA_real_, length(data))
  
  for (i in seq_len(length(data))) {
    if (i >= period + k_period) {
      
      window <- rsi_values[(i - k_period + 1):i]
      
      lowest <- min(window, na.rm = TRUE)
      highest <- max(window, na.rm = TRUE)
      
      if (highest != lowest) {
        normalized[i] <- (rsi_values[i] - lowest) /
          (highest - lowest)
      } else {
        normalized[i] <- 0
      }
    }
  }
  
  K <- rep(NA_real_, length(data))
  D <- rep(NA_real_, length(data))
  
  for (i in seq_len(length(data))) {
    if (i >= period + 2 * k_period - 1) {
      K[i] <- mean(normalized[(i - k_period + 1):i])
    }
    
    if (i >= period + 2 * k_period + d_period - 2) {
      D[i] <- mean(K[(i - d_period + 1):i])
    }
  }
  
  return(list(K = K * 100, D = D * 100))
}

prices <- c(10, 12, 11, 14, 13, 15, 16, 14, 17, 18,
            16, 19, 20, 18, 21, 22, 20, 23, 24, 22)

stoch_rsi(prices, 5, 3, 3)
