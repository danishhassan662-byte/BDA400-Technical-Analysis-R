linreg <- function(regressionSource, regressionLength, regressionOffset) {
  
  n <- length(regressionSource)
  
  if (regressionLength > n) {
    stop("Regression length cannot exceed data length")
  }
  
  if (regressionOffset >= regressionLength) {
    stop("Regression offset must be smaller than regression length")
  }
  
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset - 1)
  
  source_values <- regressionSource[start_index:end_index]
  index_values <- seq_along(source_values)
  
  mean_x <- mean(index_values)
  mean_y <- mean(source_values)
  
  slope <- sum((index_values - mean_x) *
                 (source_values - mean_y)) /
    sum((index_values - mean_x)^2)
  
  intercept <- mean_y - slope * mean_x
  
  predicted_values <- intercept + slope * index_values
  
  return(list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  ))
}
