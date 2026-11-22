stdev <- function(data) {

  if (length(data) == 0) {
    stop("Data cannot be empty")
  }

  mean_value <- sum(data) / length(data)
  diff_values <- data - mean_value
  squared_diff <- diff_values * diff_values
  variance <- sum(squared_diff) / length(data)
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}
