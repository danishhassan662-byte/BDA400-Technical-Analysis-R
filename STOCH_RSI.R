source("RSI.R")
source("SMA.R")

stoch_rsi <- function(data, period, k_period, d_period) {
  
  rsi_values <- rsi(data, period)
  
  valid_rsi <- rsi_values[!is.na(rsi_values)]
  
  if (length(valid_rsi) == 0) {
    stop("Not enough data to calculate StochRSI")
  }
  
  min_rsi <- min(valid_rsi)
  max_rsi <- max(valid_rsi)
  
  if (max_rsi == min_rsi) {
    k_values <- rep(NA_real_, length(rsi_values))
  } else {
    k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  }
  
  k_line <- sma(k_values, k_period)
  d_line <- sma(k_line, d_period)
  
  return(list(
    K = k_line,
    D = d_line
  ))
}
