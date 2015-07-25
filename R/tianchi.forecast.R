use.lm <- function(train.data,  pred.data){
  # browser()
  train.purchase <- train.data[[1]]
  train.redeem <- train.data[[2]]
  pred.purchase <- pred.data[[1]]
  pred.redeem <- pred.data[[2]]
  purchase.col.nums <- ncol(train.purchase)
  redeem.col.nums <- ncol(train.redeem)
  purchase <- colnames(train.purchase)[purchase.col.nums]
  redeem <- colnames(train.redeem)[redeem.col.nums]
  # browser()
  purchase.condition <- colnames(train.purchase)[c(-1, - purchase.col.nums)]
  purchase.formulaStr <- paste( purchase.condition, collapse='+')
  purchase.formulaStr <- paste(purchase, purchase.formulaStr, sep=' ~ ')
  redeem.condition <- colnames(train.redeem)[c(-1, -redeem.col.nums)]
  redeem.formulaStr <- paste( redeem.condition, collapse='+')
  redeem.formulaStr <- paste(redeem, redeem.formulaStr, sep=' ~ ')
  # print(purchase.formulaStr)
  # browser()
  purchase.model = lm(as.formula(purchase.formulaStr), train.purchase)
  redeem.model = lm(as.formula(redeem.formulaStr), train.redeem)
#   purchase.model = bestglm( train.purchase[, 2:col.nums], IC='AIC')$BestModels
#   redeem.model = bestglm( train.redeem[, 2:col.nums], IC='AIC')$BestModels
  # print(purchase.model)
  # print(redeem.model)
  result.data <- data.frame(report_date=pred.purchase[,1])
  result.data$purchase <- predict(purchase.model, pred.purchase, level=0.95, se.fit=F, interval = "prediction")[,1]
  result.data$redeem <- predict(redeem.model, pred.redeem, level=0.95, se.fit=F, interval = "prediction")[,1]
  # browser()
  return(result.data)
}

use.ridge <- function(train.data,  pred.data){
  train.purchase <- train.data[[1]]
  train.redeem <- train.data[[2]]
  pred.purchase <- pred.data[[1]]
  pred.redeem <- pred.data[[2]]
  purchase.col.nums <- ncol(train.purchase)
  redeem.col.nums <- ncol(train.redeem)
  purchase <- colnames(train.purchase)[purchase.col.nums]
  redeem <- colnames(train.redeem)[redeem.col.nums]
  browser()
  purchase.condition <- colnames(train.purchase)[c(-1, - purchase.col.nums)]
  purchase.formulaStr <- paste( purchase.condition, collapse='+')
  purchase.formulaStr <- paste(purchase, purchase.formulaStr, sep=' ~ ')
  redeem.condition <- colnames(train.redeem)[c(-1, -redeem.col.nums)]
  redeem.formulaStr <- paste( redeem.condition, collapse='+')
  redeem.formulaStr <- paste(redeem, redeem.formulaStr, sep=' ~ ')
  # print(purchase.formulaStr)
  # browser()
  # browser()
  purchase.model = linearRidge(as.formula(purchase.formulaStr), train.purchase ) #, nPCs = purchase.col.nums-2)
  redeem.model = linearRidge(as.formula(redeem.formulaStr), train.redeem )#, nPCs = redeem.col.nums-2)
  # print(purchase.model)
  # print(redeem.model)
  result.data <- data.frame(report_date=pred.purchase[,1])
  # browser()
  result.data$purchase <- predict(purchase.model, pred.purchase)
  result.data$redeem <- predict(redeem.model, pred.redeem)
  # browser()
  return(result.data)
}

use.rpart <- function(train.data,  pred.data){
  train.purchase <- train.data[[1]]
  train.redeem <- train.data[[2]]
  pred.purchase <- pred.data[[1]]
  pred.redeem <- pred.data[[2]]
  purchase.col.nums <- ncol(train.purchase)
  redeem.col.nums <- ncol(train.redeem)
  purchase <- colnames(train.purchase)[purchase.col.nums]
  redeem <- colnames(train.redeem)[redeem.col.nums]
  # browser()
  purchase.condition <- colnames(train.purchase)[c(-1, - purchase.col.nums)]
  purchase.formulaStr <- paste( purchase.condition, collapse='+')
  purchase.formulaStr <- paste(purchase, purchase.formulaStr, sep=' ~ ')
  redeem.condition <- colnames(train.redeem)[c(-1, -redeem.col.nums)]
  redeem.formulaStr <- paste( redeem.condition, collapse='+')
  redeem.formulaStr <- paste(redeem, redeem.formulaStr, sep=' ~ ')
  # print(purchase.formulaStr)
  # browser()
  # browser()
  purchase.model <- rpart(as.formula(purchase.formulaStr), train.purchase, control = rpart.control(cp = 0.05))
  # purchase.model <- prune(purchase.model, cp = 0.05)
  redeem.model <- rpart(as.formula(redeem.formulaStr), train.redeem, control = rpart.control(cp = 0.05) )
  # redeem.model <- prune( redeem.model, cp = 0.05)
  # print(purchase.model)
  # print(redeem.model)
  result.data <- data.frame(report_date=pred.purchase[,1])
  # browser()
  result.data$purchase <- predict(purchase.model, pred.purchase, type='vector')
  result.data$redeem <- predict(redeem.model, pred.redeem, type='vector')
  # browser()
  return(result.data)
}

use.stlf <- function(train.data,  pred.data, model.type = 'ets', fre = 7, s = 15){
  # browser()
  train.purchase <- train.data[[1]]
  train.redeem <- train.data[[2]]
  pred.purchase <- pred.data[[1]]
  pred.redeem <- pred.data[[2]]
  purchase.col.nums <- ncol(train.purchase)
  redeem.col.nums <- ncol(train.redeem)
  purchase <-train.purchase[, purchase.col.nums]
  redeem   <-train.redeem[, redeem.col.nums]
  purchase.t <- ts( purchase, frequency = fre )
  redeem.t <- ts( redeem, frequency = fre )
  horizon <- nrow(pred.purchase)
  if(model.type == 'ets'){
    purchase.model <- stlf(purchase.t, 
               h=horizon, 
               s.window=s, 
               method='ets',
               ic='bic', 
               opt.crit='mae')
    redeem.model <- stlf(redeem.t, 
                           h=horizon, 
                           s.window=s, 
                           method='ets',
                           ic='bic', 
                           opt.crit='mae')
  }else if(model.type == 'arima'){
    purchase.model <- stlf( purchase.t, 
               h=horizon, 
               s.window=s, 
               method='arima',
               ic='bic')
    redeem.model <- stlf( redeem.t, 
                            h=horizon, 
                            s.window=s, 
                            method='arima',
                            ic='bic')
  }else{
    stop('Model type must be one of ets or arima.')
  }
  # print(purchase.model)
  # print(redeem.model)
  result.data <- data.frame(report_date=pred.purchase[,1])
  result.data$purchase <- as.vector(purchase.model$mean)
  result.data$redeem <- as.vector(redeem.model$mean)
  # browser()
  return(result.data)
}

use.randomforest <- function(train.data,  pred.data,  fre = 7){
  train.purchase <- train.data[[1]]
  train.redeem <- train.data[[2]]
  pred.purchase <- pred.data[[1]]
  pred.redeem <- pred.data[[2]]
  purchase.col.nums <- ncol(train.purchase)
  redeem.col.nums <- ncol(train.redeem)
  purchase <- colnames(train.purchase)[purchase.col.nums]
  redeem <- colnames(train.redeem)[redeem.col.nums]
  # browser()
  purchase.condition <- colnames(train.purchase)[c(-1, - purchase.col.nums)]
  purchase.formulaStr <- paste( purchase.condition, collapse='+')
  purchase.formulaStr <- paste(purchase, purchase.formulaStr, sep=' ~ ')
  redeem.condition <- colnames(train.redeem)[c(-1, -redeem.col.nums)]
  redeem.formulaStr <- paste( redeem.condition, collapse='+')
  redeem.formulaStr <- paste(redeem, redeem.formulaStr, sep=' ~ ')
  # print(purchase.formulaStr)
  set.seed(12)
  # browser()
  purchase.model = randomForest(as.formula(purchase.formulaStr), train.purchase, ntree = 500)
  redeem.model = randomForest(as.formula(redeem.formulaStr), train.redeem, ntree = 500 )
  # print(purchase.model)
  # print(redeem.model)
  result.data <- data.frame(report_date=pred.purchase[,1])
  result.data$purchase <- predict(purchase.model, pred.purchase )
  result.data$redeem <- predict(redeem.model, pred.redeem )
  # browser()
  return(result.data)
}

use.gbm <- function(train.data,  pred.data,  fre = 7){
  train.purchase <- train.data[[1]]
  train.redeem <- train.data[[2]]
  pred.purchase <- pred.data[[1]]
  pred.redeem <- pred.data[[2]]
  purchase.col.nums <- ncol(train.purchase)
  redeem.col.nums <- ncol(train.redeem)
  purchase <- colnames(train.purchase)[purchase.col.nums]
  redeem <- colnames(train.redeem)[redeem.col.nums]
  # browser()
  purchase.condition <- colnames(train.purchase)[c(-1, - purchase.col.nums)]
  purchase.formulaStr <- paste( purchase.condition, collapse='+')
  purchase.formulaStr <- paste(purchase, purchase.formulaStr, sep=' ~ ')
  redeem.condition <- colnames(train.redeem)[c(-1, -redeem.col.nums)]
  redeem.formulaStr <- paste( redeem.condition, collapse='+')
  redeem.formulaStr <- paste(redeem, redeem.formulaStr, sep=' ~ ')  # print(purchase.formulaStr)
  set.seed(12)
  purchase.model = gbm(as.formula(purchase.formulaStr), data = train.purchase, n.trees = 500, distribution = 'gaussian' ,  shrinkage=0.0015)
  redeem.model = gbm(as.formula(redeem.formulaStr), data =train.redeem,  n.trees = 500, distribution = 'gaussian' ,   shrinkage=0.0015  )
  # print(purchase.model)
  # print(redeem.model)
  result.data <- data.frame(report_date=pred.purchase[,1])
  # browser()
  result.data$purchase <- predict(purchase.model, pred.purchase, n.trees = 500)
  result.data$redeem <- predict(redeem.model, pred.redeem, n.trees = 500)
  # browser()
  return(result.data)
}


use.nnet <- function(train.data,  pred.data, fre = 7){
  train.purchase <- train.data[[1]]
  train.redeem <- train.data[[2]]
  pred.purchase <- pred.data[[1]]
  pred.redeem <- pred.data[[2]]
  purchase.col.nums <- ncol(train.purchase)
  redeem.col.nums <- ncol(train.redeem)
  purchase <- colnames(train.purchase)[ purchase.col.nums]
  redeem <- colnames(train.redeem)[redeem.col.nums]
  # browser()
  purchase.max <- max(train.purchase[, purchase.col.nums] )
  purchase.min <- min(train.purchase[, purchase.col.nums] )
  redeem.max <- max(train.redeem[, redeem.col.nums] )
  redeem.min <- min(train.redeem[, redeem.col.nums] )
  # browser()
  train.purchase[, purchase.col.nums] <- (train.purchase[, purchase.col.nums] - purchase.min)/( purchase.max - purchase.min)
  train.redeem[, redeem.col.nums] <- (train.redeem[, redeem.col.nums] - redeem.min)/( redeem.max - redeem.min)
  purchase.condition <- colnames(train.purchase)[c(-1, - purchase.col.nums)]
  purchase.formulaStr <- paste( purchase.condition, collapse='+')
  purchase.formulaStr <- paste(purchase, purchase.formulaStr, sep=' ~ ')
  redeem.condition <- colnames(train.redeem)[c(-1, - redeem.col.nums)]
  redeem.formulaStr <- paste( redeem.condition, collapse='+')
  redeem.formulaStr <- paste(redeem, redeem.formulaStr, sep=' ~ ')
  # print(purchase.formulaStr)
  set.seed(12)
  # browser()
  purchase.model = nnet(as.formula(purchase.formulaStr), train.purchase,  size = 10)
  redeem.model = nnet(as.formula(redeem.formulaStr), train.redeem , size = 10 )
  # print(purchase.model)
  # print(redeem.model)
  # browser()
  result.data <- data.frame(report_date=pred.purchase[,1])
  result.data$purchase <- predict(purchase.model, pred.purchase, level=0.95, se.fit=F, interval = "prediction")*(purchase.max-purchase.min) 
                          + purchase.min
  result.data$redeem <- predict(redeem.model, pred.redeem, level=0.95, se.fit=F, interval = "prediction")*(redeem.max-redeem.min) 
  + redeem.min
  # browser()
  return(result.data)
}

use.glmnet <- function(train.data,  pred.data,  fre = 7){
  train.purchase <- train.data[[1]]
  train.redeem <- train.data[[2]]
  pred.purchase <- pred.data[[1]]
  pred.redeem <- pred.data[[2]]
  purchase.col.nums <- ncol(train.purchase)
  redeem.col.nums <- ncol(train.redeem)
  purchase <- colnames(train.purchase)[purchase.col.nums]
  redeem <- colnames(train.redeem)[redeem.col.nums]
  # browser()
  purchase.condition <- colnames(train.purchase)[c(-1, - purchase.col.nums)]
  purchase.formulaStr <- paste( purchase.condition, collapse='+')
  purchase.formulaStr <- paste(purchase, purchase.formulaStr, sep=' ~ ')
  redeem.condition <- colnames(train.redeem)[c(-1, -redeem.col.nums)]
  redeem.formulaStr <- paste( redeem.condition, collapse='+')
  redeem.formulaStr <- paste(redeem, redeem.formulaStr, sep=' ~ ')
  # print(purchase.formulaStr)
  set.seed(12)
  purchase.model = cv.glmnet( as.matrix(train.purchase[,2:(purchase.col.nums-1)]),
                              as.vector(train.purchase[,(purchase.col.nums)]), nfolds = 5 )
  redeem.model = cv.glmnet( as.matrix(train.redeem[,2:(redeem.col.nums-1)]), 
                            as.vector(train.redeem[,(redeem.col.nums)]),  nfolds = 5 )
  # print(purchase.model)
  # print(redeem.model)
  result.data <- data.frame(report_date=pred.purchase[,1])
  # browser()
  result.data$purchase <- predict(purchase.model, as.matrix(pred.purchase[,2:ncol(pred.purchase)]), s='lambda.min')
  result.data$redeem <- predict(redeem.model, as.matrix(pred.redeem[,2:ncol(pred.redeem)]), s='lambda.min' )
  # browser()
  return(result.data)
}
use.svm <- function(train.data,  pred.data){
  train.purchase <- train.data[[1]]
  train.redeem <- train.data[[2]]
  pred.purchase <- pred.data[[1]]
  pred.redeem <- pred.data[[2]]
  purchase.col.nums <- ncol(train.purchase)
  redeem.col.nums <- ncol(train.redeem)
  purchase <- colnames(train.purchase)[purchase.col.nums]
  redeem <- colnames(train.redeem)[redeem.col.nums]
  # browser()
  purchase.condition <- colnames(train.purchase)[c(-1, - purchase.col.nums)]
  purchase.formulaStr <- paste( purchase.condition, collapse='+')
  purchase.formulaStr <- paste(purchase, purchase.formulaStr, sep=' ~ ')
  redeem.condition <- colnames(train.redeem)[c(-1, -redeem.col.nums)]
  redeem.formulaStr <- paste( redeem.condition, collapse='+')
  redeem.formulaStr <- paste(redeem, redeem.formulaStr, sep=' ~ ')
  # print(purchase.formulaStr)
  # browser()
  purchase.model = svm( as.matrix(train.purchase[,2:(purchase.col.nums-1)]),
                        as.vector(train.purchase[,(purchase.col.nums)]), kernel = 'linear' )
  redeem.model = svm( as.matrix(train.redeem[,2:(redeem.col.nums-1)]), 
                      as.vector(train.redeem[,(redeem.col.nums)]), kernel = 'linear' )
  # print(purchase.model)
  # print(redeem.model)
  result.data <- data.frame(report_date=pred.purchase[,1])
  # browser()
  result.data$purchase <- predict(purchase.model, as.matrix(pred.purchase[,2:ncol(pred.purchase)]) )
  result.data$redeem <- predict(redeem.model, as.matrix(pred.redeem[,2:ncol(pred.redeem)])  )
  # browser()
  return(result.data)
}

use.extraTrees <- function(train.data,  pred.data){
  train.purchase <- train.data[[1]]
  train.redeem <- train.data[[2]]
  pred.purchase <- pred.data[[1]]
  pred.redeem <- pred.data[[2]]
  purchase.col.nums <- ncol(train.purchase)
  redeem.col.nums <- ncol(train.redeem)
  purchase <- colnames(train.purchase)[purchase.col.nums]
  redeem <- colnames(train.redeem)[redeem.col.nums]
  # browser()
  purchase.condition <- colnames(train.purchase)[c(-1, - purchase.col.nums)]
  purchase.formulaStr <- paste( purchase.condition, collapse='+')
  purchase.formulaStr <- paste(purchase, purchase.formulaStr, sep=' ~ ')
  redeem.condition <- colnames(train.redeem)[c(-1, -redeem.col.nums)]
  redeem.formulaStr <- paste( redeem.condition, collapse='+')
  redeem.formulaStr <- paste(redeem, redeem.formulaStr, sep=' ~ ')
  # print(purchase.formulaStr)
  set.seed(12)
  # browser()
  purchase.model = extraTrees( as.matrix(train.purchase[,2:(purchase.col.nums-1)]),
                               as.vector(train.purchase[,(purchase.col.nums)]), ntree = 500, nodesize = 3 )
  redeem.model = extraTrees( as.matrix(train.redeem[,2:(redeem.col.nums-1)]), 
                             as.vector(train.redeem[,(redeem.col.nums)]), ntree = 500, nodesize = 3  )
  # print(purchase.model)
  # print(redeem.model)
  result.data <- data.frame(report_date=pred.purchase[,1])
  # browser()
  result.data$purchase <- predict(purchase.model, as.matrix(pred.purchase[,2:ncol(pred.purchase)]))
  result.data$redeem <- predict(redeem.model, as.matrix(pred.redeem[,2:ncol(pred.redeem)])  )
  # browser()
  return(result.data)
}

use.knn <- function(train.data,  pred.data){
  train.purchase <- train.data[[1]]
  train.redeem <- train.data[[2]]
  pred.purchase <- pred.data[[1]]
  pred.redeem <- pred.data[[2]]
  purchase.col.nums <- ncol(train.purchase)
  redeem.col.nums <- ncol(train.redeem)
  purchase <- colnames(train.purchase)[purchase.col.nums]
  redeem <- colnames(train.redeem)[redeem.col.nums]
  # browser()
  purchase.condition <- colnames(train.purchase)[c(-1, - purchase.col.nums)]
  purchase.formulaStr <- paste( purchase.condition, collapse='+')
  purchase.formulaStr <- paste(purchase, purchase.formulaStr, sep=' ~ ')
  redeem.condition <- colnames(train.redeem)[c(-1, -redeem.col.nums)]
  redeem.formulaStr <- paste( redeem.condition, collapse='+')
  redeem.formulaStr <- paste(redeem, redeem.formulaStr, sep=' ~ ')
  # print(purchase.formulaStr)
  # browser()
#   purchase.model = extraTrees( as.matrix(train.purchase[,2:(col.nums-1)]),as.vector(train.purchase[,(col.nums)]) )
#   redeem.model = extraTrees( as.matrix(train.redeem[,2:(col.nums-1)]), as.vector(train.redeem[,(col.nums)]) )
#   print(purchase.model)
#   print(redeem.model)
  result.data <- data.frame(report_date=pred.purchase[, 1])
  # browser()
  result.data$purchase <- knn.reg(as.matrix(train.purchase[,2:(purchase.col.nums-1)]), 
                                  as.matrix(pred.purchase[,2:ncol(pred.purchase)]), 
                                  as.vector(train.purchase[,(purchase.col.nums)]), k=5 )$pred
  result.data$redeem <- knn.reg(as.matrix(train.redeem[,2:(redeem.col.nums-1)]), 
                                as.matrix(pred.redeem[,2:ncol(pred.redeem)]) ,
                                as.vector(train.redeem[,(redeem.col.nums)]), k=5  )$pred
  # browser()
  return(result.data)
}

use.interest.adjust <- function( pred_result ){
  interest.data <- feature.interest()
  ori.data <- get.ori.data()
  # browser()
}

run.forecast <- function(method, preprocess.data, ...){
  train.data <- preprocess.data[1:2]
  pred.data <- preprocess.data[3:4]
  if(method %in% methods){
    m <- get(method)
  }else{
    stop(method ,' not legal forecast option')
  }
  result.data <- m(train.data, pred.data, ...)
}
