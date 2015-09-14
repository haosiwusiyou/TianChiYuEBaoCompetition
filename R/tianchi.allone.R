source('R/tianchi.forecast.R')
source('R/tianchi.postprocess.R')
source('R/tianchi.feature.engineering.R')
source('R/tianchi.feature.selection.R')
source('R/tianchi.configuration.R')
library('forecast')  
#Forecasting Functions for Time Series and Linear Models
library('randomForest')
#Breiman and Cutler's random forests for classification and
#regression
library('nnet')
#Software for feed-forward neural networks with a single
#hidden layer, and for multinomial log-linear models.
library('glmnet')
#Extremely efficient procedures for fitting the entire lasso or elastic-net regularization path for linear regression, logistic and multinomial regression models, Poisson regression and the Cox model.
library('e1071')
#Functions for latent class analysis, short time Fourier
#transform, fuzzy clustering, support vector machines,
#shortest path computation, bagged clustering, naive Bayes
#classifier, ...
library('extraTrees')
#Classification and regression based on an ensemble of decision trees. The package also provides extensions of ExtraTrees to multi-task learning and quantile regression. Uses Java implementation of the method.
library('ridge')
#Linear and logistic ridge regression for small data sets
#and genome-wide SNP data
library('gbm')
#An implementation of extensions to Freund and
#Schapire's AdaBoost algorithm and Friedman's gradient boosting
#machine. Includes regression methods for least squares,
#absolute loss, t-distribution loss, quantile regression,
#logistic, multinomial logistic, Poisson, Cox proportional
#hazards partial likelihood, AdaBoost exponential loss,
#Huberized hinge loss, and Learning to Rank measures
library('FNN')
#Cover-tree and kd-tree fast k-nearest neighbor search algorithms and related applications
#including KNN classification, regression and information measures are implemented.
library('bestglm')
#Best subset glm using AIC, BIC, EBIC, BICq or Cross-Validation. For the normal case, the 'leaps' is used. Otherwise, a slower exhaustive search. The 'xtable' package is needed for vignette 'SimExperimentBICq.Rnw' accompanying this package.
library('FSelector')
#This package provides functions for selecting attributes
#from a given dataset. Attribute subset selection is the process
#of identifying and removing as much of the irrelevant and
#redundant information as possible.
library('caret')
#Misc functions for training and plotting classification and
#regression models.
library('rpart')
#Recursive partitioning for classification, 
#regression and survival trees.  An implementation of most of the 
#functionality of the 1984 book by Breiman, Friedman, Olshen and Stone.
preprocess.data <- run.feature.engineering(train.range, pred.range)
ori.result <- preprocess.data[[5]]

method = 'use.lm'
write(method , result.file, append = T)
# reduced.data <- run.feature.selction(method , preprocess.data )
reduced.data <- preprocess.data
lm.pred.result <- run.forecast(method, reduced.data)
run.postprocess(ori.result, lm.pred.result, result.file )

# # #### use ridgealgorithm
# method = 'use.ridge'
# write(method , result.file, append = T)
# reduced.data <- run.feature.selction(method , preprocess.data )
# rg.pred.result <- run.forecast(method, reduced.data)
# run.postprocess(ori.result, rg.pred.result, result.file )


#### use randomforest algorithm
method = 'use.randomforest'
write(method , result.file, append = T)
# reduced.data <- run.feature.selction(method , preprocess.data )
reduced.data <- preprocess.data
rf.pred.result <- run.forecast(method, reduced.data)
run.postprocess(ori.result, rf.pred.result, result.file )


# method = 'use.extraTrees'
# write(method , result.file, append = T)
# reduced.data <- run.feature.selction(method , preprocess.data )
# et.pred.result <- run.forecast(method, reduced.data)
# run.postprocess(ori.result, et.pred.result, result.file )



# method = 'use.gbm'
# write(method , result.file, append = T)
# reduced.data <- run.feature.selction(method , preprocess.data )
# gb.pred.result <- run.forecast(method, reduced.data)
# run.postprocess(ori.result, gb.pred.result, result.file )
# 
# 
# method = 'use.knn'
# write(method , result.file, append = T)
# reduced.data <- run.feature.selction(method , preprocess.data )
# kn.pred.result <- run.forecast(method, reduced.data)
# run.postprocess(ori.result, kn.pred.result, result.file )

# #### use nnet algorithm
# write('use nnet algorithm' , result.file, append = T)
# nnet.pred.result <- run.forecast('use.nnet', preprocess.data)
# # stopifnot(nrow(nnet.pred.result ) == nrow(ori.result) ,  ncol(nnet.pred.result ) == ncol(ori.result))
# run.postprocess(ori.result, nnet.pred.result, result.file )


# # #### use glmnet algorithm
method = 'use.glmnet'
write(method , result.file, append = T)
# reduced.data <- run.feature.selction(method , preprocess.data )
reduced.data <- preprocess.data
glmnet.pred.result <- run.forecast(method, reduced.data)
run.postprocess(ori.result, glmnet.pred.result, result.file )

# # #### use svm algorithm
method = 'use.svm'
write(method , result.file, append = T)
# reduced.data <- run.feature.selction(method , preprocess.data )
reduced.data <- preprocess.data
svm.pred.result <- run.forecast(method, reduced.data)
run.postprocess(ori.result, svm.pred.result, result.file )

# # ### use stlf.arima algorithm
# write('use stlf.arima algorithm' , result.file, append = T)
# arima.pred.result <- run.forecast('use.stlf', preprocess.data, model.type = 'arima')
# # stopifnot(nrow(arima.pred.result ) == nrow(ori.result) ,  ncol(arima.pred.result ) == ncol(ori.result))
# run.postprocess(ori.result, arima.pred.result, result.file )
# 
# 
# ### use stlf.ets algorithm
# write('use stlf.ets algorithm' , result.file, append = T)
# ets.pred.result <- run.forecast('use.stlf', preprocess.data, model.type = 'ets')
# # stopifnot(nrow(ets.pred.result ) == nrow(ori.result) ,  ncol(ets.pred.result ) == ncol(ori.result))
# run.postprocess(ori.result, ets.pred.result, result.file )
# # 
# # ### use stlf.arim + stlf.ets algorithm by averaging
# write('use stlf.arim + stlf.ets algorithm by averaging' , result.file, append = T)
# arima.ets.pred.result <- make.combine(list(arima.pred.result, ets.pred.result)  )
# run.postprocess(ori.result, arima.ets.pred.result, result.file )

# browser()
## use stlf.arim + stlf.ets + lm algorithm by averaging
# write('use stlf.arim + stlf.ets + lm algorithm by averaging' , result.file, append = T)
# lm.arima.ets.pred.result <- make.combine(list(arima.pred.result, ets.pred.result, lm.pred.result)  )
# run.postprocess(ori.result, lm.arima.ets.pred.result, result.file )
# 
# 
# ### use stlf.arim + stlf.ets + lm algorithm + random forest by averaging
# write('use stlf.arim + stlf.ets + lm algorithm + random forest by averaging' , result.file, append = T)
# rf.lm.arima.ets.pred.result <- make.combine(list(arima.pred.result, ets.pred.result, lm.pred.result, rf.pred.result)  )
# run.postprocess(ori.result, rf.lm.arima.ets.pred.result, result.file )


# ### use stlf.arim + stlf.ets +  random forest by averaging
# write('use stlf.arim + stlf.ets +  random forest by averaging' , result.file, append = T)
# rf.lm.arima.ets.pred.result <- make.combine(list(arima.pred.result, ets.pred.result,  rf.pred.result)  )
# run.postprocess(ori.result, rf.lm.arima.ets.pred.result, result.file )
# 
# ####'use glmnet  + random forest by averaging
# write('use glmnet  + random forest by averaging' , result.file, append = T)
# rf.lm.arima.ets.pred.result <- make.combine(list(glmnet.pred.result,   rf.pred.result)  )
# run.postprocess(ori.result, rf.lm.arima.ets.pred.result, result.file )
# 
# ####'use glmnet  + lm by averaging
# write('use glmnet  + svm by averaging' , result.file, append = T)
# rf.lm.arima.ets.pred.result <- make.combine(list(glmnet.pred.result,   svm.pred.result)  )
# run.postprocess(ori.result, rf.lm.arima.ets.pred.result, result.file )

####'use glmnet  + lm + random forest by averaging
write('use svm  +  random forest by averaging' , result.file, append = T)
rf.lm.arima.ets.pred.result <- make.combine(list(svm.pred.result, rf.pred.result)  )
run.postprocess(ori.result, rf.lm.arima.ets.pred.result, result.file )

####'use glmnet  + svm + random forest by averaging
write('use glmnet  + svm + random forest by averaging' , result.file, append = T)
rf.lm.arima.ets.pred.result <- make.combine(list(glmnet.pred.result,   svm.pred.result , rf.pred.result)  )
run.postprocess(ori.result, rf.lm.arima.ets.pred.result, result.file )
# cat( paste( rf.lm.arima.ets.pred.result[,2], collapse = ','),'\n')
# cat( paste( rf.lm.arima.ets.pred.result[,3], collapse = ','),'\n')

# ####'use glmnet  + svm + random forest + extraTrees by averaging
# write('use glmnet  + svm + random forest + extraTrees by averaging' , result.file, append = T)
# rf.lm.arima.ets.pred.result <- make.combine(list(glmnet.pred.result,   svm.pred.result , rf.pred.result, et.pred.result)  )
# run.postprocess(ori.result, rf.lm.arima.ets.pred.result, result.file )
# 
# ####'use glmnet  + svm + random forest + extraTress + ridge by averaging
# write('use glmnet  + svm + random forest + extraTrees + ridge by averaging' , result.file, append = T)
# rf.lm.arima.ets.pred.result <- make.combine(list(glmnet.pred.result,   svm.pred.result , rf.pred.result, et.pred.result, rg.pred.result)  )
# run.postprocess(ori.result, rf.lm.arima.ets.pred.result, result.file )

####'use glmnet  + svm + random forest + extraTress + ridge + gb by averaging
# write('use glmnet  + svm + random forest + extraTrees + ridge + gbm by averaging' , result.file, append = T)
# rf.lm.arima.ets.pred.result <- make.combine(list(glmnet.pred.result,   svm.pred.result , rf.pred.result, et.pred.result, rg.pred.result, gb.pred.result)  )
# run.postprocess(ori.result, rf.lm.arima.ets.pred.result, result.file )

####'use glmnet  + svm + random forest + extraTress + ridge + knn by averaging
# write('use glmnet  + svm + random forest + extraTrees + ridge + knn by averaging' , result.file, append = T)
# rf.lm.arima.ets.pred.result <- make.combine(list(glmnet.pred.result,   svm.pred.result , rf.pred.result, et.pred.result, rg.pred.result, kn.pred.result)  )
# run.postprocess(ori.result, rf.lm.arima.ets.pred.result, result.file )


####'use glmnet  + svm + random forest + extraTress + ridge + knn + gbm by averaging
# write('use glmnet  + svm + random forest + extraTrees + ridge + knn + gbm by averaging' , result.file, append = T)
# rf.lm.arima.ets.pred.result <- make.combine(list(glmnet.pred.result,   svm.pred.result , rf.pred.result, et.pred.result, rg.pred.result, kn.pred.result, gb.pred.result)  )
# run.postprocess(ori.result, rf.lm.arima.ets.pred.result, result.file )

# ####'use glmnet  + svm + random forest + extraTress + ridge + knn + arima + ets by averaging
# write('use glmnet  + svm + random forest + extraTrees + ridge + lm + knn by averaging' , result.file, append = T)
# rf.lm.arima.ets.pred.result <- make.combine(list(glmnet.pred.result,   svm.pred.result , rf.pred.result, et.pred.result, rg.pred.result, lm.pred.result, kn.pred.result)  )
# run.postprocess(ori.result, rf.lm.arima.ets.pred.result, result.file )
# cat( paste( rf.lm.arima.ets.pred.result[,2], collapse = ','),'\n')
# cat( paste( rf.lm.arima.ets.pred.result[,3], collapse = ','),'\n')
# # browser()
# history.data <- get.ori.data(data.path )
# # rf.lm.arima.ets.pred.result[, 1] <- format(rf.lm.arima.ets.pred.result[, 1], format = '%Y%m%d')
# names(rf.lm.arima.ets.pred.result) <- names(history.data)
# output.data1 <- rbind(history.data, rf.lm.arima.ets.pred.result  )
# write.csv(output.data1,'result/output.data1_old2.csv', row.names = F )

####'use glmnet  + svm + random forest + extraTress + ridge + knn + arima + ets by averaging
write('svm + lm + randomforest + glmet by averaging' , result.file, append = T)
rf.lm.arima.ets.pred.result <- make.combine(list(   svm.pred.result,  lm.pred.result, glmnet.pred.result, rf.pred.result)  )
run.postprocess(ori.result, rf.lm.arima.ets.pred.result, result.file )
cat( paste( rf.lm.arima.ets.pred.result[,2], collapse = ','),'\n')
cat( paste( rf.lm.arima.ets.pred.result[,3], collapse = ','),'\n')
# browser()
history.data <- get.ori.data(user.balance.ori )
# rf.lm.arima.ets.pred.result[, 1] <- format(rf.lm.arima.ets.pred.result[, 1], format = '%Y%m%d')
names(rf.lm.arima.ets.pred.result) <- names(history.data)
output.data2 <- rbind(history.data, rf.lm.arima.ets.pred.result  )
write.csv(output.data2, paste(result.dir, 'output.data2_old2.csv', sep=""), row.names = F )
####'use glmnet  + svm + random forest  by averaging
# write('use glmnet  + svm + random forest + lm  by averaging' , result.file, append = T)
# rf.lm.arima.ets.pred.result <- make.combine(list(glmnet.pred.result,   svm.pred.result , rf.pred.result, lm.pred.result)  )
# run.postprocess(ori.result, rf.lm.arima.ets.pred.result, result.file )

# 
# ####'use glmnet  + svm + random forest + stlf.arima + stlf.ets by averaging
# write('use glmnet  + svm + random forest + stlf.arima + stlf.ets + lm  by averaging' , result.file, append = T)
# rf.lm.arima.ets.pred.result <- make.combine(list(glmnet.pred.result,   svm.pred.result , rf.pred.result,lm.pred.result , arima.pred.result, ets.pred.result)  )
# run.postprocess(ori.result, rf.lm.arima.ets.pred.result, result.file )

