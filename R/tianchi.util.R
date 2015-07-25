
# turn numberic attribute' na to value 0
na2zero <- function( x ){
  if (is.numeric( x ))
    pos <- which(is.na(x))
  if (length(pos) > 0) {
    x[pos] <- 0
  }
  return(x)
} 

