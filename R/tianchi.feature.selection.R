# feature.selection <- function(method, all.features.data){
#   purchase.full.features <- all.features.data[[1]]
#   redeem.full.features <- all.features.data[[2]]
#   date.range <- purchase.full.features[, 1]
#   score.purchase.before <- 0 
#   score.redeem.before <- 0
#   unique.month <- unique( as.numeric( format(date.range, '%m' ) ))
#   selected.purchase.features <- data.frame(report_date = date.range )
#   selected.redeem.features <- data.frame(report_date = date.range )
#   num.purchase.features <- ncol( purchase.full.features )
#   num.redeem.features <- ncol( redeem.full.features )
#   cancidate.purchase.features <- names(purchase.full.features )[-1, -num.purchase.features]
#   selected.purchase.features <- c( names(purchase.full.features )[ num.purchase.features ]  )
#   selected.redeem.features <- c( names(purchase.full.features )[ num.redeem.features ]  )
#   # cancidate.redeem.features <- seq(2, ncol(redeem.full.features  ) )
#   while(  T ){
#     for(  f in selected.purchase.features){
#       temp.purchase[, f] <- purchase.full.features[, f]
#       temp.redeem[, f] <- redeem.full.features[, f]
#       for( i in unique.month){
#         train.month <- setdiff(unique.month, i )
#         test.month <- i 
#         temp.purchase.features <- c( f, selected.purchase.features)
#         temp.redeem.features <- c( f, selected.redeem.features)
#         train.purchase.features <- purchase.full.features[ as.numeric(format( date.range, '%m' )) %in% train.month,  temp.purchase.features]
#         train.redeem.features  <- redeem.full.features[ as.numeric(format( date.range, '%m' )) %in% train.month,  temp.redeem.features]
#         test.purchase.features <- purchase.full.features[ as.numeric(format( date.range, '%m' )) %in% test.month,  temp.purchase.features]
#         test.redeem.features   <- redeem.full.features[ as.numeric(format( date.range, '%m' )) %in% test.month,  temp.redeem.features]
#       } 
#     }
#   }
# }

index.based.selection <- function(datas, forecast.method ){
  train.purchase <- datas[[1]]
  train.redeem <- datas[[2]]
  pred.purchase <- datas[[3]]
  pred.redeem <- datas[[4]]
  train.purchase.num <- ncol( train.purchase )
  train.redeem.num  <- ncol( train.redeem )
#   browser()
#   if( forecast.method == 'use.ridge'){
#     # browser()
#   }
#   if( !(forecast.method  %in% c('use.randomforest', 'use.extraTrees', 'use.lm') )  ){
#      control <- trainControl(method="repeatedcv", number=5, repeats=3)
#   if( forecast.method == 'use.svm'){
#      m = "svmLinear"
#   }else if(forecast.method == 'use.glmnet' ){
#      m = "glmnet"
#   }else if(forecast.method == 'use.ridge' ){
#      m = "ridge"
#   }else if( forecast.method == 'use.knn'){
#      m = "knn"
#   }else if(forecast.method == 'use.lm'){
#      m = "lm"
#   }else if( forecast.method == 'use.gbm' ){
#      m = "gbm"
#   }
#   
#     purchase.model <-  train( as.formula( paste( names(train.purchase)[train.purchase.num], '~', 
#                         paste(names(train.purchase)[2:(train.purchase.num-1)], collapse = '+'  ), 
#                         sep = ' '  )), data = train.purchase, method = m , preProcess = 'scale', trControl = control) 
#     redeem.model <-  train(as.formula( paste( names( train.redeem )[ train.redeem.num], '~', 
#                         paste( names( train.redeem )[2 : ( train.redeem.num-1 ) ], collapse = '+'  ), 
#                         sep = ' '  ) ), data = train.redeem, method = m, preProcess = 'scale', trControl = control )
#   
#   # browser()
#     purchase.importance <- varImp( purchase.model, scale = F )
#     redeem.importance <- varImp( redeem.model, scale = F )
#     # browser()
#     # purchase.importance$importance <- data.frame(purchase.importance$importance[-1,])
#     # redeem.importance$importance <- data.frame(redeem.importance$importance[-1,])
#     # cat( row.names( purchase.importance$importance), '\n')
#     sel.purchase.att <-  row.names(purchase.importance$importance)[ purchase.importance$importance[, 1] 
#                                                 > 2.5*mean(purchase.importance$importance[, 1]) ]
#     sel.redeem.att <-   row.names(redeem.importance$importance)[  redeem.importance$importance[, 1] 
#                                                 > 2.5*mean(redeem.importance$importance[, 1])]
# #     cat('forecast.method:', forecast.method, '\n')
# #     cat('sel.purchase.att:', sel.purchase.att, '\n'  )
# #     cat('sel.redeem.att:', sel.redeem.att, '\n'  )
#     if( sum(sel.purchase.att %in% names(train.purchase)[1]) == 0 ){
#        sel.purchase.att <- c(names(train.purchase)[1], sel.purchase.att )
#     }
#     if( sum(sel.purchase.att %in% names(train.purchase)[ train.purchase.num ])== 1 ){
#        sel.purchase.att <- setdiff( sel.purchase.att, names(train.purchase)[ train.purchase.num ] )
#     }
#     if( sum((sel.redeem.att %in% names(train.redeem)[1])) == 0 ){
#       sel.redeem.att <- c(names(train.redeem)[1], sel.redeem.att )
#     }
#     if( sum(sel.redeem.att %in% names(train.redeem)[ train.redeem.num ]) == 1 ){
#       sel.redeem.att <- setdiff( sel.redeem.att, names(train.redeem)[ train.redeem.num ] )
#     }
#     # cat( sel.purchase.att , '\n')
#   }else if( forecast.method == 'use.lm'){
#         cat( ' cfs', '\n')
#         sel.purchase.att <- c(names( train.purchase )[ 1 ],  cfs( as.formula( paste( names(train.purchase)[train.purchase.num], '~', 
#                                                                                paste(names(train.purchase)[2:(train.purchase.num-1)], collapse = '+'  ), 
#                                                                                sep = ' '  )), train.purchase ) )
#         sel.redeem.att <- c(names( train.redeem )[ 1 ] ,cfs( as.formula( paste( names( train.redeem )[ train.redeem.num], '~', 
#                                                                               paste( names( train.redeem )[2 : ( train.redeem.num-1 ) ], collapse = '+'  ), 
#                                                                               sep = ' '  ) ), train.redeem ) )
#    } else{
#     set.seed(12)
#     cat( ' random forest', '\n')
#     purchase.atts.importance <- random.forest.importance( as.formula( paste( names(train.purchase)[train.purchase.num], '~', 
#                                                                                  paste(names(train.purchase)[2:(train.purchase.num-1)], collapse = '+'  ), 
#                                                                                  sep = ' '  )), train.purchase, importance.type = 1 )[, 1]
#     redeem.atts.importance <- random.forest.importance( as.formula( paste( names( train.redeem )[ train.redeem.num], '~', 
#                                                                                paste( names( train.redeem )[2 : ( train.redeem.num-1 ) ], collapse = '+'  ), 
#                                                                                sep = ' '  ) ), train.redeem, importance.type = 1 )[, 1]
#     sel.purchase.att <- c(names( train.purchase )[ 1 ], names(train.purchase)[
#           which( purchase.atts.importance>2.5*mean(purchase.atts.importance) )+1] )
#     
#     sel.redeem.att <- c(names( train.redeem )[ 1 ] , names(train.redeem)[
#           which( redeem.atts.importance> 2.5*mean(redeem.atts.importance) )+1]  )
#   }
  
  
  if( forecast.method %in% c( 'use.svm', 'use.lm', 'use.ridge', 'use.knn','use.glmnet'  ) ){
    cat( ' cfs', '\n')
    sel.purchase.att <- c(names( train.purchase )[ 1 ],  cfs( as.formula( paste( names(train.purchase)[train.purchase.num], '~', 
                                                                           paste(names(train.purchase)[2:(train.purchase.num-1)], collapse = '+'  ), 
                                                                           sep = ' '  )), train.purchase ) )
    sel.redeem.att <- c(names( train.redeem )[ 1 ] ,cfs( as.formula( paste( names( train.redeem )[ train.redeem.num], '~', 
                                                                          paste( names( train.redeem )[2 : ( train.redeem.num-1 ) ], collapse = '+'  ), 
                                                                          sep = ' '  ) ), train.redeem ) )
  
  }else if( forecast.method %in% c('use.glmnet', 'use.randomforest', 'use.extraTrees' , 'use.gbm', 'use.knn'  ) ){  ## random forest importance
    set.seed(12)
    cat( ' random forest', '\n')
    purchase.atts.importance <- random.forest.importance( as.formula( paste( names(train.purchase)[train.purchase.num], '~', 
                                                                             paste(names(train.purchase)[2:(train.purchase.num-1)], collapse = '+'  ), 
                                                                             sep = ' '  )), train.purchase, importance.type = 1 )[, 1]
    redeem.atts.importance <- random.forest.importance( as.formula( paste( names( train.redeem )[ train.redeem.num], '~', 
                                                                           paste( names( train.redeem )[2 : ( train.redeem.num-1 ) ], collapse = '+'  ), 
                                                                           sep = ' '  ) ), train.redeem, importance.type = 1 )[, 1]
    sel.purchase.att <- c(names( train.purchase )[ 1 ], names(train.purchase)[
      which( purchase.atts.importance>2.5 )+1] )
    sel.redeem.att <- c(names( train.redeem )[ 1 ] , names(train.redeem)[
      which( redeem.atts.importance> 2.5 )+1]  )
    
  }else{
    sel.purchase.att <- names(train.purchase)[c(-train.purchase.num)]
    sel.redeem.att <- names(train.redeem)[c(-train.redeem.num)]
  }
  cat('forecast.method:', forecast.method, '\n')
  cat('sel.purchase.att:', sel.purchase.att, '\n'  )
  cat('sel.redeem.att:', sel.redeem.att, '\n'  )
  pred.purchase <- pred.purchase[, sel.purchase.att ]
  pred.redeem <- pred.redeem[, sel.redeem.att ]
  train.purchase <- train.purchase[, c( sel.purchase.att, names(train.purchase)[train.purchase.num])  ]
  train.redeem <- train.redeem[,  c( sel.redeem.att, names(train.redeem)[train.redeem.num])  ]
  
  # browser()
  return( list(train.purchase, train.redeem, pred.purchase, pred.redeem, datas[[5]]  ))
}
wrapper.based.method<- function(datas, forecast.method){
  train.purchase <- datas[[1]]
  train.redeem <- datas[[2]]
  pred.purchase <- datas[[3]]
  pred.redeem <- datas[[4]]
  train.purchase.num <- ncol( train.purchase )
  train.redeem.num  <- ncol( train.redeem )
  purchase.full.atts <- names(train.purchase)[c(-1, -train.purchase.num)]
  redeem.full.atts <- names(train.redeem)[c(-1, -train.redeem.num)]
}
run.feature.selction <- function( forecast.method, datas   ){
  reduced.datas <- index.based.selection(datas, forecast.method )
  return( reduced.datas)
}