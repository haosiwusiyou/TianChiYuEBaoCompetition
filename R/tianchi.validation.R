source('rScript/tianchi.forecast.R')
source('rScript/tianchi.postprocess.R')
source('rScript/tianchi.feature.engineering.R')
source('rScript/tianchi.feature.selection.R')
source('rScript/tianchi.configuration.R')
source( 'rScript/tianchi.util.R' )
library('forecast')
library('randomForest')
library('nnet')
library('glmnet')
library('e1071')
library('extraTrees')
library('ridge')
library('gbm')
library('FNN')
library('bestglm')
library('FSelector')
library('caret')
library('rpart')

unique.month <- unique( as.numeric( format( valid.range, '%m' ) ))
# methods <- c( 'use.randomforest', 'use.nnet', 'use.glmnet','use.svm', 'use.lm', 'use.stlf', 'use.extraTrees', 'use.ridge', 'use.gbm', 'use.knn')
# methods <- c('use.svm', 'use.randomforest', 'use.glmnet',    'use.extraTrees','use.ridge', 'use.knn', 'use.lm', 'use.rpart')
methods <- c('use.svm', 'use.randomforest', 'use.glmnet',     'use.lm')
for( m in methods){
  write( m , result.file, append = T)
  score.sum = 0 
  # reduced.data <- run.feature.selction(m , preprocess.data )
  # reduced.data <- preprocess.data
  if( m == 'use.ridge'){
    # browser()
  }
  for( i in unique.month){
    train.month <- setdiff(unique.month, i )
    test.month <- i 
    train.range <- valid.range[as.numeric(format( valid.range, '%m' )) %in% train.month]
    test.range <- valid.range[as.numeric(format( valid.range, '%m' )) %in% test.month]
    validation.data <- run.feature.engineering( train.range, test.range )
    ori.result <- validation.data[[5]]
    pred.result <- run.forecast( m , validation.data)
    score.sum =  score.sum  + sum(run.postprocess(ori.result, pred.result, result.file ))
  }
  cat(m,': ', score.sum, '\n'  )
}

