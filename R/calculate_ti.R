calculate_ti <- function(x) {
  
  x <- sort(x, decreasing = T)
  ti <- NULL
  
  for (i in 1:length(x)) {
    
    ti[i] <- x[i] * prod((x[1:i-1] + 1)^-1)
    
  }
  
  return(sum(ti))
}