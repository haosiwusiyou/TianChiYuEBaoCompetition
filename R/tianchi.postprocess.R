post.evaluation <- function(real.data , pred.data, result.file , need.detail=F){
  # browser()
  date.range <- pred.data[, 1]
  pred.purchase <- pred.data[, 2]
  pred.redeem <- pred.data[, 3]
  real.purchase <- real.data[, 2]
  real.redeem <- real.data[, 3]
  date.months <- as.numeric(format(date.range,  '%m'))
  unique.month <- unique(date.months)
  total.score <- 0
  for( i in 1:length(unique.month)){
    write(paste('month ',  unique.month[i],  ': ',sep=''), result.file, append = T)
    pos <- date.months == unique.month[i]
    purchase.score <- post.evaluation.fourmula(real.purchase[pos],  pred.purchase[pos] )
    redeem.score <- post.evaluation.fourmula(real.redeem[pos],  pred.redeem[pos] )
    write(paste( 0.45*sum(purchase.score)+0.55*sum(redeem.score),' ',sep=''), result.file, append = T)
    if(need.detail){
      write(paste('purchase: ',sep=''), result.file, append = T)
      write(paste('summary purchase score:',  sum(purchase.score),  '\n ',sep=''), result.file, append = T)
      write(paste('purchase score:',  purchase.score,  ' ',sep=''), result.file, append = T)
      write(paste('redeem:',set=''), result.file, append = T)
      write(paste('summary redeem score:',  sum(redeem.score),  '\n ',sep=''), result.file, append = T)
      write(paste('redeem score:',  sum(redeem.score),  ' ',sep=''), result.file, append = T)
    }
    total.score = total.score + 0.45*sum(purchase.score) + 0.55*sum(redeem.score) 
  }
  write(paste('total score ', total.score, '\n\n',sep=''), result.file, append = T)
  return( c(total.score) )
}
post.evaluation.fourmula <- function(real ,  pred ){
  #   error <- abs(pred.data - real.data)/real.data 
  #   error[error>0.3] <- 10
  #   score <- 10*exp(-10*error)
  #   return(score)
  errors <- (real - pred)
  errors.abs <- abs(errors)
  errors.relative <- errors.abs / real
  # then count it as 1.
  x <- errors.relative
  x = 8*x - 0.4;
  score3 <- 10 * (-(exp(3*x) - exp(x))/(exp(3*x) + exp(-x)) + 0.98)/1.19 
  score3 <- ifelse(score3 < 0, 0, score3)
  score <- score3
  return(score)
}
make.combine <- function( datas  ){
  # browser()
  stopifnot(is.list(datas))
  nums <- length(datas)
  combine <- datas[[1]]
  if(nums > 1 ){
    for( i in 2:nums ){
      combine[,2:3] <-  combine[,2:3] + datas[[i]][,2:3]
    } 
  }
  combine[,2:3] <-  combine[,2:3]/nums
  return (combine)
}
run.postprocess <- function(real.data , pred.data,  result.file ,... ){
  scores <- post.evaluation(real.data , pred.data,  result.file)
  return (scores)
}