
crossover <- function(arr1, arr2) {
  
  if (length(arr1) != length(arr2)) {
    stop("Both arrays must have the same length")
  }
  
  result <- rep(FALSE, length(arr1))
  
  for (i in 2:length(arr1)) {
    result[i] <- arr1[i] > arr2[i] &&
      arr1[i - 1] <= arr2[i - 1]
  }
  
  return(result)
}
