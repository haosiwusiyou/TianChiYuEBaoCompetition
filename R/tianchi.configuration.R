data.dir    <- 'data/'
user.balance.ori <- paste(data.dir, 'user_balance_sum.csv',sep="")
day.share.mod    <- paste(data.dir, 'mfd_day_share_interest_mod.csv',sep="")
bank.shibor.mod  <- paste(data.dir, 'mfd_bank_shibor_mod.csv', sep="")


result.dir  <- 'result/'
result.log  <- paste(result.dir, 'log/', sep='')
result.file <- paste(result.dir,'result_record',format(Sys.time(),'%Y_%m_%d_%H_%M_%S'), '.txt' ,sep = '')
if(!dir.exists(result.dir)){
  dir.create(result.dir)
}
if(!file.exists(result.file)){
  file.create(result.file)
}


train.range <- seq( as.Date('2014-04-01'),as.Date('2014-07-31'), by = 1)
pred.range  <- seq(as.Date('2014-08-01'), as.Date('2014-08-31'), by = 1)  #if pred.range  between 2013-07-01 and 2014-09-01, then we can gain validation result.
# valid.range <- seq( as.Date('2014-04-01'),as.Date('2014-08-31'), by = 1)

# other
form = '%Y%m%d'  # time format 
methods <- c( 'use.randomforest', 'use.nnet', 'use.glmnet','use.svm', 'use.lm', 'use.stlf', 'use.extraTrees', 'use.ridge', 'use.gbm', 'use.knn', 'use.rpart')
# features <- c('feature.week' )