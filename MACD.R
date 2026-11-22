source("EMA.R")

macd <- function(data, fast_period, slow_period, signal_period) {
  
  fast_ema <- ema(data, fast_period)
  slow_ema <- ema(data, slow_period)
  
  macd_line <- fast_ema - slow_ema
  signal_line <- ema(macd_line, signal_period)
  histogram <- macd_line - signal_line
  
  return(list(
    MACD = macd_line,
    Signal = signal_line,
    Histogram = histogram
  ))
}

prices <- 100 + cumsum(rep(c(1, 2, -1, 3, -2), 10))

result <- macd(prices, 12, 26, 9)

result$MACD
result$Signal
result$Histogram
