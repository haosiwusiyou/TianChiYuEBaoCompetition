preprocess.svd <- function(train.data, threshold.value = 0.95){
  # browser()
  s <-  svd(train.data[,2:ncol(train.data)])
  leave.num <- min(which(cumsum(s$d)/sum(s$d)> threshold.value))
  train.data[,2:ncol(train.data)] <- as.matrix(s$u[,1:leave.num])%*%diag(s$d)[1:leave.num,1:leave.num]%*% t( as.matrix(s$v[,1:leave.num])) 
  return (train.data)
}
preprocess.pca <- function(train.data, threshold.value = 0.95){
  # browser()
  p <-  princomp(train.data[,2:ncol(train.data)], cor = T)
  leave.num <- min(which(cumsum(p$sdev)/sum(p$sdev)> threshold.value))
  pca.train.data <- data.frame( report_date = train.data[, 1] )
  pca.train.data[,2:(leave.num+1)] <- as.matrix(train.data[,2:ncol(train.data)]) %*% p$loadings[,1:leave.num]
  return (pca.train.data)
}

feature.week <- function(date.range ){
  zeros.week <- rep(0,   length(date.range))
  week.data.frame <- data.frame(report_date=date.range,  sun=zeros.week,  mon=zeros.week,  tue=zeros.week,  
                                wed=zeros.week,  thu=zeros.week,  fri=zeros.week,  sat= zeros.week) 
                                # workdays= zeros.week, weekends =  zeros.week)
  col.pos <- as.numeric(format(date.range,  '%w'))
  
  for( i in 1:length(date.range)){
    week.data.frame[i,  2+col.pos[i]] <- 1
}
#   browser()
#   interest.data <- feature.interest()
#   interest.data <- interest.data[ interest.data[, 1] %in% date.range , ]
#   for( i in 1:length(date.range)){
#     week.data.frame[i,  2+col.pos[i]] <- interest.data[i, 2]
#   }
  # browser()
#   for( i in 2:ncol(week.data.frame)){
#     week.data.frame[, i] <- as.factor( week.data.frame[,i])
#   }
  return (week.data.frame)
}

feature.interest.week <- function(date.range, diff.nums = c( )){
  zeros.week <- rep(0,   length(date.range))
  interest.week.data.frame <- data.frame(report_date=date.range,  sun.interest.value=zeros.week,  mon.interest.value=zeros.week,  tue.interest.value=zeros.week,  
                                wed.interest.value=zeros.week,  thu.interest.value=zeros.week,  fri.interest.value=zeros.week,  sat.interest.value= zeros.week
                                ) 
  # interest.week.data.frame <- data.frame(report_date=date.range) 
  # workdays= zeros.week, weekends =  zeros.week)
  col.pos <- as.numeric(format(date.range,  '%w'))
  # browser()
  # interest.week.data.frame[, 'interest' ] <- 
  interest.data <- feature.interest()
  # browser()
  interest.data1 <- interest.data[ (interest.data[, 1]+1) %in% date.range , ]
  interest.data1[, 2] <- (interest.data1[, 2] - min(interest.data1[, 2]))/(max(interest.data1[, 2])-min(interest.data1[, 2])) + 1
  
  for( i in 1:length(date.range)){
    interest.week.data.frame[i,  2+col.pos[i]] <- (exp(interest.data1[i, 2]))/exp(1)
    # interest.week.data.frame[i,  2+col.pos[i]] <- ((interest.data1[i, 2]))
  }
  # browser()
  if( length( diff.nums )> 0){
    diff.interest.att <- paste('diff.interest', diff.nums , sep='')
    for( i in 1:length(diff.nums)){
      diff.num <- diff.nums[i]
      temp.interest.data <- interest.data
      temp.interest.data[(diff.num+1):nrow(interest.data) ,2] <- diff( interest.data[,2], diff.num )
      temp.interest.data <- temp.interest.data[-seq(1,diff.num),]
      temp.interest.data <- temp.interest.data[ (temp.interest.data[, 1]) %in% date.range , ]
      interest.week.data.frame[, diff.interest.att[i]] <- temp.interest.data[,2]
    }
  }
  # for( i in 1:nrow(interest.data1)){
    # interest.week.data.frame[i,  9+col.pos[i]] <- (interest.data1[i, 2])
#     if(interest.data1[i, 2]>= 0){
#       interest.week.data.frame[i,  9+col.pos[i]] <- exp(interest.data1[i, 2])/exp(1)
#     }else{
#       interest.week.data.frame[i,  9+col.pos[i]] <- -exp(-interest.data1[i, 2])/exp(1)
#     }
  # }
  # browser()
  return (interest.week.data.frame)
}

## divid the month into several part,   the default interval is 10
feature.month <- function(date.range, ori.data,   interval = 7 ){
  # browser()
  zeros.month <- rep(0,   length(date.range))
  month.part.num <- round(31/interval) 
  # month.data.frame <- data.frame(report_date=date.range)
#   month.att <- paste('monthAtt',month.part.num,  1:month.part.num,  sep='')
#   month.data.frame[month.att] <- rep(0,   length(date.range))
  month.purchase.data <- data.frame(report_date=date.range)
  month.redeem.data <- data.frame(report_date=date.range)
  ori.data[, 2] <- (ori.data[, 2]-min(ori.data[, 2]))/(max(ori.data[, 2])-min(ori.data[, 2])) + 1
  ori.data[, 3] <- (ori.data[, 3]-min(ori.data[, 3]))/(max(ori.data[, 3])-min(ori.data[, 3])) + 1
  col.pos <- (as.numeric(format(date.range,  '%d'))-1)%/%interval + 1
  if( max(col.pos)> month.part.num ){
    col.pos[col.pos==max(col.pos)] <- month.part.num
  }
  for( i in col.pos ){
    month.purchase.data[ col.pos == i ,'month.purchase'] <- sum(ori.data[ col.pos == i , 2] ) / sum( ori.data[, 2] )
    month.redeem.data[ col.pos == i ,'month.redeem'] <- sum(ori.data[ col.pos == i , 3] ) / sum( ori.data[, 3] )
  }
  return( list(month.purchase.data, month.redeem.data ))
#   for( i in 1:length(date.range)){
#     month.data.frame[i,  1+col.pos[i]] <- 1
#   }
  # return( month.data.frame)
}

## only holiday information,   only consider the within holday days
feature.holiday <- function(date.range,   need.before=T,   need.after=T, need.weight= T, effect.factor.be =1.25, effect.factor.af =1.5 ){
  # browser()
  fes <- c(as.Date('2014-04-05'),  as.Date('2014-05-01'),   as.Date('2014-06-02'),      as.Date('2014-09-06'),    as.Date('2014-10-01'))
  fes.len <- c(3,  3,    1,  3,    3)
  #   fes <- c(as.Date('2014-04-05'),  as.Date('2014-05-01'),    as.Date('2014-09-06'),   as.Date('2014-10-01'))
  #   fes.len <- c(3,  3,   3,    3)
  # fes.importance<- c(2,3, 1,  1,  1,  1,  1, 2, 1,  3)
  fes.importance<- c(2,3, 1, 2, 3)  
  fes.effect.be <- fes.importance   
  fes.effect.af <- fes.importance - 1  
  fes.within <- paste('fesWi',  1:max(fes.len),  sep='')
  fes.att <- fes.within
  fes.effect <- fes.len
  if(need.before){
    fes.before <- paste('fesBe',  1:max(fes.effect.be),  sep='')
    fes.att <- c(fes.before,  fes.att)
    fes.effect <- fes.effect + fes.effect.be
  }
  if(need.after){
    # browser()
    fes.after <- paste('fesAf',  1:max(fes.effect.af),  sep='')
    fes.att <- c(fes.att,   fes.after)
    fes.effect <- fes.effect + fes.effect.af
  }
  fes.data <- data.frame(report_date=date.range)
  fes.data[fes.att] <- rep(0,  length(date.range))
  date.matrix <-matrix( rep(date.range,   length(fes)),  ncol =length(fes))
  fes.matrix <- matrix( rep(fes,   each = length(date.range) ),   ncol = length(fes) )
  if(need.before){
    time.diff <- date.matrix  - fes.matrix   + 1 +  matrix( rep(fes.effect.be,   each = length(date.range) ),   ncol = length(fes) )
  }else{
    time.diff <- date.matrix  - fes.matrix   + 1 
  }
  fes.len.matrix <- matrix(rep(fes.effect,   each = length(date.range) ),  ncol=length(fes))
  pos <-  time.diff <= fes.len.matrix & time.diff>0
  for( i in 1:length(date.range)){
    if( sum( pos[i,  ]) > 0 ){
      # browser()
      ith.diff <- time.diff[i,  pos[i, ]]
      ith.within <- fes.len[pos[i, ]]
      ith.before <- fes.effect.be[pos[i, ]]
      ith.after <- fes.effect.af[pos[i, ]]
      ith.importance <- fes.importance[pos[i, ]]
      if( !need.weight){
        for( j in 1:length(ith.diff)){
          fes.data[i, ith.diff[j]+ 1] <- fes.data[i, ith.diff[j]+ 1]  + 1
        }
      }else{
        for( j in 1:length(ith.diff)){
          # browser()
#           if(need.before && need.after){
#             if(ith.diff[j] <= ith.before[j]){
#               fes.data[i, fes.before[ith.diff[j] ] ] <- fes.data[i, fes.before[ith.diff[j] ] ]  + (1 - exp(- ith.diff[j]))*ith.importance[j]
#             }else if(ith.diff[j] <= ith.before[j]+ ith.within[j]){
#               # browser()
#               fes.data[i, fes.within[ ith.diff[j]- ith.before[j] ] ] <- fes.data[i, fes.within[ ith.diff[j]- ith.before[j] ] ] + (1 - exp( -(ith.before[j]+ ith.within[j]  - ith.diff[j] + 1 )))*ith.importance[j]
#             }else{
#               # browser()
#               fes.data[i, fes.after[ ith.diff[j]- ith.before[j]- ith.within[j] ] ] <- fes.data[i, fes.after[ ith.diff[j]- ith.before[j]- ith.within[j] ] ] + (1 - exp( -(ith.before[j]+ ith.within[j] + ith.after[j] - ith.diff[j] + 1)))*ith.importance[j]
#             }
#           }else if(need.before){
#             if(ith.diff[j] <= ith.before[j]){
#               fes.data[i, fes.before[ ith.diff[j]  ] ] <- fes.data[i, fes.before[ ith.diff[j]  ] ]  + (1- exp(- ith.diff[j]))*ith.importance[j]
#             }else {
#               # browser()
#               fes.data[i, fes.within[ ith.diff[j]- ith.before[j]  ] ] <- fes.data[i, fes.within[ ith.diff[j]- ith.before[j]  ] ] + (1- exp(-(  ith.before[j]+ ith.within[j]  - ith.diff[j] + 1)))*ith.importance[j]
#             }
#           }else if(need.after){
#             if(ith.diff[j] <= ith.within[j]){
#               fes.data[i, fes.within[ith.diff[j]] ] <- fes.data[i, fes.within[ith.diff[j]] ]  + (1- exp(-(ith.within[j] - ith.diff[j] + 1)))*ith.importance[j]
#             }else {
#               # browser()
#               fes.data[i, fes.after[ith.diff[j]-ith.within[j] ] ] <- fes.data[i, fes.after[ith.diff[j]-ith.within[j] ] ] + (1- exp(-( ith.after[j] + ith.within[j]  - ith.diff[j] + 1)))*ith.importance[j]
#             }
#           }else{
#             fes.data[i, fes.within[ith.diff[j]]] <- fes.data[i, fes.within[ith.diff[j]]] + (1 -exp(-(ith.within[j]  - ith.diff[j] + 1)))*ith.importance[j]
#           }
          # browser()
          if(need.before && need.after){
            if(ith.diff[j] <= ith.before[j]){
              fes.data[i, fes.before[ith.diff[j] ] ] <- fes.data[i, fes.before[ith.diff[j] ] ]  + exp(-(  ith.diff[j]/(ith.before[j] +1  )  ))/exp(1) *ith.importance[j]
            }else if(ith.diff[j] <= ith.before[j]+ ith.within[j]){
              # browser()
              fes.data[i, fes.within[ ith.diff[j]- ith.before[j] ] ] <- fes.data[i, fes.within[ ith.diff[j]- ith.before[j] ] ] + exp((-(ith.before[j]+ ith.within[j]  - ith.diff[j] + 1 )/(ith.within[j] + 1  )))/exp(1) *ith.importance[j]
            }else{
              # browser()
              fes.data[i, fes.after[ ith.diff[j]- ith.before[j]- ith.within[j] ] ] <- fes.data[i, fes.after[ ith.diff[j]- ith.before[j]- ith.within[j] ] ] + 1
            }
          }else if(need.before){
            if(ith.diff[j] <= ith.before[j]){
              fes.data[i, fes.before[ ith.diff[j]  ] ] <- fes.data[i, fes.before[ ith.diff[j]  ] ]  + exp(((ith.diff[j])/(ith.before[j]+1)))/exp(1)*ith.importance[j]
            }else {
              # browser()
              fes.data[i, fes.within[ ith.diff[j]- ith.before[j]  ] ] <- fes.data[i, fes.within[ ith.diff[j]- ith.before[j]  ] ] + exp((-(  ith.before[j]+ ith.within[j]  - ith.diff[j] + 1)/(ith.within[j]+1)))/exp(1)*ith.importance[j]
            }
          }else if(need.after){
            if(ith.diff[j] <= ith.within[j]){
              fes.data[i, fes.within[ith.diff[j]] ] <- fes.data[i, fes.within[ith.diff[j]] ]  + exp((-(ith.within[j] - ith.diff[j] + 1)/(ith.within[j]+1)))/exp(1)*ith.importance[j]
            }else {
              # browser()
              fes.data[i, fes.after[ith.diff[j]-ith.within[j] ] ] <- fes.data[i, fes.after[ith.diff[j]-ith.within[j] ] ]  + 1#exp((-( ith.after[j] + ith.within[j]  - ith.diff[j] + 1)/(ith.after[j]+1)))/exp(1)*ith.importance[j]
            }
          }else{
            fes.data[i, fes.within[ith.diff[j]]] <- fes.data[i, fes.within[ith.diff[j]]] + exp((-(ith.within[j]  - ith.diff[j] + 1)/(ith.within[j]+1)))/exp(1)*ith.importance[j]
          }  
        }      
        }
    }
  }
  importance.fes <- data.frame(report_date= fes.data[,1])
  if(need.before)
    importance.fes$fes_be <- apply(as.matrix( fes.data[, fes.before]), 1, sum)
  importance.fes$fes_wi <-  apply(as.matrix( fes.data[, fes.within]), 1, sum)
  if(need.after)
    importance.fes$fes_af <- apply(as.matrix( fes.data[, fes.after]), 1, sum)
  # browser()
  return(importance.fes)
}
feature.interest <- function(interest.file ='data/mfd_day_share_interest_mod.csv' ,horizon = 30 ,model='ets' ){
  interest.data <- read.csv(interest.file)[c('report_date','mfd_7daily_yield')]
  interest.data[,1] <- as.Date(interest.data[,1])
  interest.t <- ts(interest.data[, 2], frequency = 7)
  interest.stlf.ets <- stlf(interest.t, h=horizon, s.window=15, method = model,ic='bic')
  interest.all <- c( interest.stlf.ets$fitted,  interest.stlf.ets$mean)
  interest.all <- (interest.all - min(interest.all))/(max(interest.all) - min(interest.all)) + 1
  interest.result <- data.frame(report_date=c(interest.data[,1], seq( max(interest.data[,1])+1, max(interest.data[,1])+horizon, by =1)), 
                                # mfd_7daily_yield = interest.all,
                                # mfd_7daily_yield2 = interest.all^2,
                                mfd_7daily_yield_exp = exp(interest.all)-interest.all
                                # mfd_7daily_yield_exp2 = (exp(interest.all)-interest.all)^2
                                )
  return(interest.result)
}
# 
feature.history <- function(date.range, ori.data, date.max.withdraw = 0, week.max.withdraw = c(4) ){
    history.att.date <- paste( 'month.ago.date', 1:date.max.withdraw, sep='' ) 
    history.att.week <- paste( 'month.ago.week', week.max.withdraw, sep='' ) 
    history.purchase <- data.frame(report_date = date.range)
    history.redeem <-  data.frame(report_date = date.range)
    month.day <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    foregoing <- rep(0 , length(date.range ))
    # browser()
    ori.data[, 2] <- (ori.data[, 2]-min(ori.data[, 2]))/(max(ori.data[, 2])-min(ori.data[, 2])) + 1
    ori.data[, 3] <- (ori.data[, 3]-min(ori.data[, 3]))/(max(ori.data[, 3])-min(ori.data[, 3])) + 1
    if( date.max.withdraw > 0){
      for( i in 1:date.max.withdraw){
        changed.month <-  as.numeric( format( date.range, '%m'  )) - i
        changed.month[changed.month< 1 ] <- changed.month[changed.month < 1 ] + 12 
        foregoing <-  foregoing +  month.day[ changed.month ] ;
        sel.dates <- date.range - foregoing
        for( j in 1:length(sel.dates)){
          history.purchase[j, history.att.date[i]] <- ori.data[ ori.data[,1] %in% sel.dates[j] , 2]
          history.redeem[j, history.att.date[i]] <- ori.data[  ori.data[,1] %in% sel.dates[j], 3]
        }
      }
    }
    # browser()
    if( length(week.max.withdraw) > 0){
      history.purchase[, history.att.week[1]] <- 0
      history.redeem[, history.att.week[1]] <- 0
      for( i in 1:length(week.max.withdraw)){
          history.purchase[, history.att.week[1]] <- history.purchase[, history.att.week[1]] +  ori.data[ ori.data[,1]  %in% (date.range - ( 7*(week.max.withdraw[i]) ) ), 2]
          history.redeem[, history.att.week[1]] <- history.redeem[, history.att.week[1]] + ori.data[  ori.data[,1]  %in% (date.range - (7*(week.max.withdraw[i])) ), 3]
      }
      history.purchase[, history.att.week[1]] <- history.purchase[, history.att.week[1]]/length(week.max.withdraw)
      history.redeem[, history.att.week[1]]  <- history.redeem[, history.att.week[1]] /length(week.max.withdraw)
    }
    
    return( list(history.purchase, history.redeem ) )
  
}

feature.week.month.group <- function(train.range, full.range, ori.data, interval.month = 15 ){
  ori.data <- ori.data[ori.data[, 1] %in% train.range,  ]
  train.week <- as.numeric(format(train.range, '%w'))
  train.month <- (as.numeric(format(train.range, '%d'))-1)%/%interval.month
  full.week <- as.numeric(format(full.range, '%w'))
  full.month <- (as.numeric(format(full.range, '%d'))-1)%/%interval.month
  ori.data[, 2] <- (ori.data[, 2]-min(ori.data[, 2]))/(max(ori.data[, 2])-min(ori.data[, 2])) + 1
  ori.data[, 3] <- (ori.data[, 3]-min(ori.data[, 3]))/(max(ori.data[, 3])-min(ori.data[, 3])) + 1
  month.part.num <- round(31/interval.month) -1
  if( max(train.month)> month.part.num ){
    train.month[train.month==max(train.month)] <- month.part.num
  }
  if( max(full.month)> month.part.num ){
    full.month[full.month==max(full.month)] <- month.part.num
  }
  week.month.group.purchase <- data.frame(report_date = full.range) 
  week.month.group.redeem <- data.frame(report_date = full.range) 
  # browser()
  for( week in  unique(train.week) ){
    for( mon in unique(train.month) ){
       train.values.pos <- which( train.week == week & train.month == mon )
       full.values.pos <- which( full.week == week & full.month == mon )
       purchase.value <- ori.data[ train.values.pos, 2 ]
       redeem.value <- ori.data[ train.values.pos, 3 ]
#        week.month.group.purchase[full.values.pos, 'len' ] <- length(purchase.value)
#        week.month.group.redeem[full.values.pos, 'len' ] <- length(redeem.value)
       week.month.group.purchase[full.values.pos, 'mean' ] <- mean(purchase.value)
       week.month.group.redeem[full.values.pos, 'mean' ] <- mean(redeem.value)
       week.month.group.purchase[full.values.pos, 'max' ] <- max(purchase.value)
       week.month.group.redeem[full.values.pos, 'max' ] <- max(redeem.value)
       week.month.group.purchase[full.values.pos, 'min' ] <- min(purchase.value)
       week.month.group.redeem[full.values.pos, 'min' ] <- min(redeem.value)
       week.month.group.purchase[full.values.pos, 'std' ] <- sd(purchase.value)
       week.month.group.redeem[full.values.pos, 'std' ] <- sd(redeem.value)
       week.month.group.purchase[full.values.pos, 'median' ] <- median(purchase.value)
       week.month.group.redeem[full.values.pos, 'median' ] <- median(redeem.value)
    }
  }
  # browser()
  return(list(week.month.group.purchase, week.month.group.redeem) )
}

feature.lm <- function(train.data,  pred.data){
  # browser()
  train.purchase <- train.data[[1]]
  train.redeem <- train.data[[2]]
  pred.purchase <- pred.data[[1]]
  pred.redeem <- pred.data[[2]]
  col.nums <- ncol(train.purchase)
  purchase <- colnames(train.purchase)[col.nums]
  redeem <- colnames(train.redeem)[col.nums]
  # browser()
  train.purchase[, col.nums] <- (train.purchase[, col.nums] - min(train.purchase[, col.nums]))/(max(train.purchase[, col.nums])- min(train.purchase[, col.nums])) + 1
  train.redeem[, col.nums] <- (train.redeem[, col.nums] - min(train.redeem[, col.nums]))/(max(train.redeem[, col.nums])- min(train.redeem[, col.nums])) + 1
  purchase.condition <- colnames(train.purchase)[c(-1, - col.nums)]
  purchase.formulaStr <- paste( purchase.condition, collapse='+')
  purchase.formulaStr <- paste(purchase, purchase.formulaStr, sep=' ~ ')
  redeem.condition <- colnames(train.redeem)[c(-1, - col.nums)]
  redeem.formulaStr <- paste( redeem.condition, collapse='+')
  redeem.formulaStr <- paste(redeem, redeem.formulaStr, sep=' ~ ')
  # print(purchase.formulaStr)
  purchase.model = lm(as.formula(purchase.formulaStr), train.purchase)
  redeem.model = lm(as.formula(redeem.formulaStr), train.redeem)
  # print(purchase.model)
  # print(redeem.model)
  result.purchase.data <- data.frame(report_date= sort(unique(c( train.purchase[, 1], pred.purchase[, 1] ) ) )  )
  result.redeem.data <- data.frame(report_date= sort(unique(c( train.purchase[, 1], pred.purchase[, 1] ) ) )  )
  result.purchase.data$lm_purchase <-c(purchase.model$fitted  ,predict(purchase.model, pred.purchase, level=0.95, se.fit=F, interval = "prediction")[,1])
  result.redeem.data$lm_redeem <- c(redeem.model$fitted   ,predict(redeem.model, pred.redeem, level=0.95, se.fit=F, interval = "prediction")[,1])
  # browser()
  return(list(result.purchase.data, result.redeem.data ))
}

feature.glmnet <- function(train.data,  pred.data,  num.lambda = 5   ){
  train.purchase <- train.data[[1]]
  train.redeem <- train.data[[2]]
  pred.purchase <- pred.data[[1]]
  pred.redeem <- pred.data[[2]]
  col.nums <- ncol(train.purchase)
  pred.purchase <- rbind(train.purchase[,1:(col.nums-1)], pred.purchase)
  pred.redeem <- rbind(train.redeem[,1:(col.nums-1)], pred.redeem)
  purchase <- colnames(train.purchase)[col.nums]
  redeem <- colnames(train.redeem)[col.nums]
  # browser()
  purchase.max <- max(train.purchase[,col.nums] )
  purchase.min <- min(train.purchase[,col.nums] )
  redeem.max <- max(train.redeem[,col.nums] )
  redeem.min <- min(train.redeem[,col.nums] )
  # browser()
  train.purchase[,col.nums] <- (train.purchase[,col.nums] - purchase.min)/( purchase.max - purchase.min) + 1
  train.redeem[,col.nums] <- (train.redeem[,col.nums] - redeem.min)/( redeem.max - redeem.min) + 1
  purchase.condition <- colnames(train.purchase)[c(-1, - col.nums)]
  purchase.formulaStr <- paste( purchase.condition, collapse='+')
  purchase.formulaStr <- paste(purchase, purchase.formulaStr, sep=' ~ ')
  redeem.condition <- colnames(train.redeem)[c(-1, - col.nums)]
  redeem.formulaStr <- paste( redeem.condition, collapse='+')
  redeem.formulaStr <- paste(redeem, redeem.formulaStr, sep=' ~ ')
  # print(purchase.formulaStr)
  purchase.model = glmnet( as.matrix(train.purchase[,2:(col.nums-1)]),as.vector(train.purchase[,(col.nums)]) )
  redeem.model = glmnet( as.matrix(train.redeem[,2:(col.nums-1)]), as.vector(train.redeem[,(col.nums)]) )
  # print(purchase.model)
  # print(redeem.model)
  result.purchase.data1 <- data.frame(report_date=pred.purchase[,1])
  result.redeem.data1 <- data.frame(report_date=pred.purchase[,1])
  # browser()
  result.purchase.data2 <-  data.frame(predict(purchase.model, as.matrix(pred.purchase[,2:ncol(pred.purchase)]), s=seq(0,0.1,0.03) ))
  result.redeem.data2 <-  data.frame(predict(redeem.model, as.matrix(pred.redeem[,2:ncol(pred.redeem)]) , s=seq(0,0.1,0.03)))
  result.purchase.data <- cbind(result.purchase.data1, result.purchase.data2 )
  result.redeem.data <- cbind(result.redeem.data1, result.redeem.data2 )
  # browser()
  return( list(result.purchase.data, result.redeem.data ) )
}

feature.svm <- function(train.data,  pred.data){
  train.purchase <- train.data[[1]]
  train.redeem <- train.data[[2]]
  pred.purchase <- pred.data[[1]]
  pred.redeem <- pred.data[[2]]
  col.nums <- ncol(train.purchase)
  purchase <- colnames(train.purchase)[col.nums]
  redeem <- colnames(train.redeem)[col.nums]
  # browser()
    purchase.max <- max(train.purchase[,col.nums] )
    purchase.min <- min(train.purchase[,col.nums] )
    redeem.max <- max(train.redeem[,col.nums] )
    redeem.min <- min(train.redeem[,col.nums] )
  # browser()
    train.purchase[,col.nums] <- (train.purchase[,col.nums] - purchase.min)/( purchase.max - purchase.min) + 1
    train.redeem[,col.nums] <- (train.redeem[,col.nums] - redeem.min)/( redeem.max - redeem.min) + 1
#   purchase.condition <- colnames(train.purchase)[c(-1, - col.nums)]
#   purchase.formulaStr <- paste( purchase.condition, collapse='+')
#   purchase.formulaStr <- paste(purchase, purchase.formulaStr, sep=' ~ ')
#   redeem.condition <- colnames(train.redeem)[c(-1, - col.nums)]
#   redeem.formulaStr <- paste( redeem.condition, collapse='+')
#   redeem.formulaStr <- paste(redeem, redeem.formulaStr, sep=' ~ ')
#   print(purchase.formulaStr)
  # browser()
  purchase.model = svm( as.matrix(train.purchase[,2:(col.nums-1)]),as.vector(train.purchase[,(col.nums)]) )
  redeem.model = svm( as.matrix(train.redeem[,2:(col.nums-1)]), as.vector(train.redeem[,(col.nums)]) )
  # print(purchase.model)
  # print(redeem.model)
  # result.data <- data.frame(report_date=pred.purchase[,1])
  # browser()
#   result.data$purchase <- predict(purchase.model, as.matrix(pred.purchase[,2:ncol(pred.purchase)]))
#   result.data$redeem <- predict(redeem.model, as.matrix(pred.redeem[,2:ncol(pred.redeem)]) )
  # browser()
  # return(result.data)
  result.purchase.data <- data.frame(report_date= sort(unique(c( train.purchase[, 1], pred.purchase[, 1] ) ) )  )
  result.redeem.data <- data.frame(report_date= sort(unique(c( train.redeem[, 1], pred.redeem[, 1] ) ) )  )
  result.purchase.data$svm_purchase <-c(purchase.model$fitted  ,predict(purchase.model, as.matrix(pred.purchase[,2:ncol(pred.purchase)])))
  result.redeem.data$svm_redeem <- c(redeem.model$fitted   ,predict(redeem.model, as.matrix(pred.redeem[,2:ncol(pred.redeem)]) ))
  # browser()
  return(list(result.purchase.data, result.redeem.data ))
  
  
}

# feature.stlf <- function(train.time, pred.time,  fre = 7, s = 15){
#   # browser()
#   ori.data <- get.ori.data()
#   ori.data <- ori.data[ori.data[ ,1]%in% train.time,  ]
#   horizon <- length(pred.time)
#   purchase <- ori.data[,2]
#   redeem <- ori.data[,3]
#   purchase <- (purchase - min(purchase))/(max(purchase) - min(purchase) )
#   redeem <- (redeem - min(redeem))/(max(redeem) - min(redeem) )
#   purchase.t <- ts( purchase, frequency = fre )
#   redeem.t <- ts( redeem, frequency = fre )
#   ets.purchase.model <- stlf(purchase.t, 
#                            h=horizon, 
#                            s.window=s, 
#                            method='ets',
#                            ic='bic', 
#                            opt.crit='mae')
#   ets.redeem.model <- stlf(redeem.t, 
#                          h=horizon, 
#                          s.window=s, 
#                          method='ets',
#                          ic='bic', 
#                          opt.crit='mae')
#   
#   arima.purchase.model <- auto.arima( purchase.t, 
#                             h=horizon, 
#                             s.window=s, 
#                             method='arima',
#                             ic='bic')
#   arima.redeem.model <- stlf( redeem.t, 
#                           h=horizon, 
#                           s.window=s, 
#                           method='arima',
#                           ic='bic')
#   result.data <- data.frame(report_date= c(ori.data[,1],seq(max(ori.data[,1])+1, max(ori.data[,1])+horizon, by = 1) ))
#   ets.purchase.value <- c(ets.purchase.model$fitted,  ets.purchase.model$mean)
#   ets.redeem.value <- c(ets.redeem.model$fitted,  ets.redeem.model$mean)
#   arima.purchase.value <- c(arima.purchase.model$fitted,  arima.purchase.model$mean)
#   arima.redeem.value <- c(arima.redeem.model$fitted,  arima.redeem.model$mean)
#   # browser()
#   result.data$purchase <- apply(matrix( c(ets.purchase.value, arima.purchase.value), ncol = 2), 1, mean)
#   result.data$redeem <- apply(matrix(c(ets.redeem.value, arima.redeem.value) , ncol = 2), 1, mean)
#   return(result.data)
# }

feature.selection <- function(train.purchase, train.){
  
}

feature.enginearing1 <- function(train.date.range, predict.date.range, ori.data){
  train.begin <- min(train.date.range)
  train.end <- max(train.date.range)
  pred.begin <- min(predict.date.range)
  pred.end <- max(predict.date.range)
  time.range <- seq( min(train.begin, pred.begin), max( train.end,pred.end),  by=1 )
  week.data <- feature.week(time.range)
  month7.data <- feature.month(time.range, ori.data,  interval = 7 )
  holiday.data <- feature.holiday(time.range, need.before = T, need.after = T )
  interest.data <- feature.interest()
  # interest.data <- feature.interest.week(time.range )
  # history.data <- feature.history(time.range, ori.data  )
  week.month.data <- feature.week.month.group( train.date.range, time.range, ori.data )
  # week.data <- merge(week.data, month5.data, by='report_date')
  week.holiday.data <- merge( week.data, holiday.data, by= 'report_date' )
  # week.holiday.data <- week.data
  holiday.interest.data <- merge( week.holiday.data, interest.data, by ='report_date')
  # holiday.interest.data <- week.holiday.data
  # browser()
#   holiday.interest.history.purchase.data <- merge(holiday.interest.data,  history.data[[1]], by ='report_date')
#   holiday.interest.history.redeem.data <- merge(holiday.interest.data,  history.data[[2]], by ='report_date')
  holiday.interest.history.purchase.data <- holiday.interest.data
  holiday.interest.history.redeem.data <- holiday.interest.data
  
  holiday.interest.history.purchase.data <- merge(holiday.interest.history.purchase.data,  month7.data[[1]], by ='report_date')
  holiday.interest.history.redeem.data <- merge(holiday.interest.history.redeem.data,  month7.data[[2]], by ='report_date')
#   holiday.interest.history.week.month.purchase.data <- merge(holiday.interest.history.purchase.data,  week.month.data[[1]], by ='report_date')
#   holiday.interest.history.week.month.redeem.data <- merge(holiday.interest.history.redeem.data,  week.month.data[[2]], by ='report_date')
#   
  holiday.interest.history.week.month.purchase.data <- holiday.interest.history.purchase.data
  holiday.interest.history.week.month.redeem.data <- holiday.interest.history.redeem.data
  num.fea <- ncol( holiday.interest.history.week.month.purchase.data )
#   holiday.interest.history.week.month.purchase.data[, num.fea+1:(num.fea-1) ] <- preprocess.svd(holiday.interest.history.week.month.purchase.data[, 1:num.fea])[, 2:num.fea] 
#   holiday.interest.history.week.month.redeem.data[ , num.fea+1:(num.fea-1) ] <- preprocess.svd(holiday.interest.history.week.month.redeem.data[, 1:num.fea])[, 2:num.fea]
#   holiday.interest.history.week.month.purchase.data[, 2:(num.fea) ] <- preprocess.svd(holiday.interest.history.week.month.purchase.data[, 1:num.fea])[, 2:num.fea] 
#   holiday.interest.history.week.month.redeem.data[ , 2:(num.fea) ] <- preprocess.svd(holiday.interest.history.week.month.redeem.data[, 1:num.fea])[, 2:num.fea]

#   num.fea <- ncol( holiday.interest.history.week.month.purchase.data )
#   purchase.features <- preprocess.pca( holiday.interest.history.week.month.purchase.data[, 1:num.fea] ) 
#   redeem.features <- preprocess.pca(  holiday.interest.history.week.month.purchase.data[, 1:num.fea]  )
#   holiday.interest.history.week.month.purchase.data <- merge(holiday.interest.history.week.month.purchase.data, purchase.features, by='report_date' )
#   holiday.interest.history.week.month.redeem.data <- merge(holiday.interest.history.week.month.redeem.data, redeem.features, by='report_date' )
  
  
  # browser()
  
  #   holiday.interest.history.week.month.purchase.data <- holiday.interest.history.week.month.purchase.data
#   holiday.interest.history.week.month.redeem.data <- holiday.interest.history.week.month.redeem.data
  # browser()
  # week.month5 <- merge(week.data, month5.data, by='report_date')
  # week.month5 <- week.data
  # week.month5.holiday <- merge(week.month5, holiday.data, by='report_date')
  # week.month5.holiday <- week.month5
  # week.month5.holiday.interest <- merge(week.month5.holiday, interest.week.data, by='report_date')
  # week.month5.holiday.interest <-week.month5.holiday 
  # week.month5.holiday.interest.history.purchase <- merge(week.month5.holiday.interest, history.data[[1]], by='report_date')
  # week.month5.holiday.interest.history.purchase <- week.month5.holiday.interest
  # week.month5.holiday.interest.history.redeem <- merge(week.month5.holiday.interest, history.data[[2]], by='report_date')
  # week.month5.holiday.interest.history.redeem <- week.month5.holiday.interest
#   week.month5.holiday.interest.history.purchase <- preprocess.svd(week.month5.holiday.interest.history.purchase)
#   week.month5.holiday.interest.history.redeem <- preprocess.svd(week.month5.holiday.interest.history.redeem)
  # browser()
  feature.data <- list(holiday.interest.history.week.month.purchase.data,  holiday.interest.history.week.month.redeem.data)
  train.purchase <- merge(feature.data[[1]][feature.data[[1]][,1] %in% train.date.range, ] , ori.data[,c(1,2)], by='report_date')
  train.redeem <- merge(feature.data[[2]][feature.data[[2]][,1] %in% train.date.range, ] , ori.data[,c(1,3)], by='report_date')
  pred.purchase <- feature.data[[1]][ feature.data[[1]][,1] %in% predict.date.range , ]
  pred.redeem <- feature.data[[2]][ feature.data[[2]][,1] %in% predict.date.range , ]
  ori.verify.data <- ori.data[ori.data[,1] %in% predict.date.range ,  ]
  train.purchase.num <- ncol( train.purchase )
  train.redeem.num  <- ncol( train.redeem )
  
  # browser()
#   sel.purchase.att <- c(names( train.purchase )[ 1 ], cfs( as.formula( paste( names(train.purchase)[train.purchase.num], '~', 
#                                               paste(names(train.purchase)[2:(train.purchase.num-1)], collapse = '+'  ), 
#                                               sep = ' '  )), train.purchase ) )
#   sel.redeem.att <- c(names( train.redeem )[ 1 ] , cfs( as.formula( paste( names( train.redeem )[ train.redeem.num], '~', 
#                                               paste( names( train.redeem )[2 : ( train.redeem.num-1 ) ], collapse = '+'  ), 
#                                               sep = ' '  ) ), train.redeem ) )
#   purchase.atts.importance <- oneR( as.formula( paste( names(train.purchase)[train.purchase.num], '~', 
#                                   paste(names(train.purchase)[2:(train.purchase.num-1)], collapse = '+'  ), 
#                                   sep = ' '  )), train.purchase )[, 1]
#   redeem.atts.importance <- oneR( as.formula( paste( names( train.redeem )[ train.redeem.num], '~', 
#                                   paste( names( train.redeem )[2 : ( train.redeem.num-1 ) ], collapse = '+'  ), 
#                                   sep = ' '  ) ), train.redeem )[, 1]
#   sel.purchase.att <- c(names( train.purchase )[ 1 ], names(train.purchase)[
#                                       which( purchase.atts.importance> 0 )+1] )
#   sel.redeem.att <- c(names( train.redeem )[ 1 ] , names(train.redeem)[
#                                       which( redeem.atts.importance> 0 )+1]  )
#   pred.purchase <- pred.purchase[, sel.purchase.att ]
#   pred.redeem <- pred.redeem[, sel.redeem.att ]
#   train.purchase <- train.purchase[, c( sel.purchase.att, names(train.purchase)[train.purchase.num])  ]
#   train.redeem <- train.redeem[,  c( sel.redeem.att, names(train.redeem)[train.redeem.num])  ] 
#   browser()
  return(list(train.purchase, train.redeem, pred.purchase, pred.redeem, ori.verify.data ) )
}

feature.enginearing2 <- function(preprocess.data, ori.data ){
  train.data <- preprocess.data[1:2]
  pred.data <- preprocess.data[3:4]
  train.time <- train.data[[1]][, 1]
  pred.time <- pred.data[[1]][, 1]
  time.begin <- min(train.time, pred.time)
  time.end <- max(train.time, pred.time)
  time.range <- seq(time.begin, time.end, by = 1 )
  
#   #### fitted and pred result from stlf.arima, stlf.ets
#   stlf.data <- feature.stlf(train.time, pred.time )
#   stlf.data <- stlf.data[stlf.data[,1] %in% time.range, ]
  #### fitted and pred result from lm
  lm.data <- feature.lm(train.data,pred.data)
  ### fitted and pred result from glmnet
  glmnet.data <- feature.glmnet(train.data,pred.data)
  ### fitted and pred result from svm
  svm.data <- feature.svm(train.data,pred.data)
  # browser()
  #### interest.data
  interest.data <- feature.interest()
  interest.data <- interest.data[interest.data[,1] %in% time.range, ]
  interest.data[,2] <- (interest.data[,2] - min(interest.data[,2]))/(max(interest.data[, 2])- min(interest.data[, 2]))
  ### ori.data
  # ori.data <- get.ori.data()
  ori.data <- ori.data[ori.data[,1] %in% time.range, ]
  # browser()
  purchase.features <- merge(lm.data[[1]], glmnet.data[[1]], by='report_date')
  redeem.features <- merge(lm.data[[2]], glmnet.data[[2]], by='report_date')
  purchase.features <- merge(purchase.features, svm.data[[1]], by='report_date')
  redeem.features <- merge(redeem.features, svm.data[[2]], by='report_date')
  # purchase.features <- preprocess.svd( purchase.features )
  # redeem.features <- preprocess.svd( redeem.features )
  train.purchase <- purchase.features[purchase.features[, 1]%in% train.time ,  ] 
  train.redeem <- redeem.features[redeem.features[, 1]%in% train.time ,  ]
  train.purchase$purchase <- ori.data[ori.data[ ,1]%in% train.time,  2]
  train.redeem$redeem <- ori.data[ori.data[ ,1]%in% train.time,  3]
  pred.purchase <- purchase.features[purchase.features[, 1]%in% pred.time ,  ] 
  pred.redeem <- redeem.features[redeem.features[, 1]%in% pred.time ,  ]
  # browser()
#   train.purchase <- data.frame(report_date = train.time,  
#                                lm.fit_pred= lm.data[lm.data[,1]%in% train.time,2],
#                                lm.fit_pred1= lm.data[lm.data[,1]%in% train.time,2],
#                                # lm.fit_pred_interest= lm.data[lm.data[,1]%in% train.time,2] - interest.data[interest.data[,1]%in% train.time, 2],
#                                # stlf.fit_pred= stlf.data[lm.data[,1]%in% train.time,2],
#                                # stlf.fit_pred_interest= stlf.data[stlf.data[,1]%in% train.time,2] - interest.data[interest.data[,1]%in% train.time, 2],
#                                purchase = ori.data[ ori.data[, 1] %in% train.time,2]
#                                )
#   train.redeem <- data.frame(report_date = train.time,  
#                                lm.fit_pred= lm.data[lm.data[,1]%in% train.time,3],
#                                lm.fit_pred1= lm.data[lm.data[,1]%in% train.time,3],
#                                # lm.fit_pred_interest = lm.data[lm.data[,1]%in% train.time,3] + interest.data[interest.data[,1]%in% train.time, 2],
#                                # stlf.fit_pred= stlf.data[stlf.data[,1]%in% train.time,3],
#                                # stlf.fit_pred_interest= stlf.data[stlf.data[,1]%in% train.time,3] + interest.data[interest.data[,1]%in% train.time, 2],
#                                redeem = ori.data[ ori.data[, 1] %in% train.time,3]
#                               )
#   pred.purchase <- data.frame(report_date = pred.time,  
#                                         lm.fit_pred= lm.data[lm.data[,1]%in% pred.time,2], 
#                                         lm.fit_pred1= lm.data[lm.data[,1]%in% pred.time,2]
#                                         # lm.fit_pred_interest= lm.data[lm.data[,1]%in% pred.time,2] + interest.data[interest.data[,1]%in% pred.time, 2],
#                                         # stlf.fit_pred= stlf.data[lm.data[,1]%in% pred.time,2]
#                                         # stlf.fit_pred_interest= stlf.data[stlf.data[,1]%in% pred.time,2] + interest.data[interest.data[,1]%in% pred.time, 2]
#                                         )
#   
#   pred.redeem <- data.frame(report_date = pred.time,  
#                                      lm.fit_pred= lm.data[lm.data[,1]%in% pred.time,3],
#                                      lm.fit_pred1= lm.data[lm.data[,1]%in% pred.time,3]
#                                      # lm.fit_pred_interest = lm.data[lm.data[,1]%in% pred.time,3] + interest.data[interest.data[,1]%in% pred.time, 2],
#                                      # stlf.fit_pred= stlf.data[lm.data[,1]%in% pred.time,3]
#                                      # stlf.fit_pred_interest= stlf.data[stlf.data[,1]%in% pred.time,2] + interest.data[interest.data[,1]%in% pred.time, 2]
#                                     )
  verify.ori.data <-  ori.data[ori.data[,1]%in% pred.time, ]
  # browser()
  return(list(train.purchase, train.redeem, pred.purchase, pred.redeem, verify.ori.data ) )
}

feature.merge <- function( datas ){
  num.datas <- length(datas)
  num.subdatas <- length(datas[[1]])
  # browser()
  for( i in 1:num.subdatas){
    for( j in 2:num.datas){
      datas[[1]][[i]] <- merge(datas[[1]][[i]], datas[[j]][[i]], by ='report_date' ) 
    }
  }
  return(datas[[1]] )
}
get.ori.data <- function(train.file ){
  ori.data <- read.csv(train.file)
  ori.data[,1] <- as.Date(as.character( ori.data[,1] ), format= '%Y%m%d')
  # browser()
#   purchast.t <- ts(ori.data[,2], frequency = 7)
#   redeem.t <- ts(ori.data[,3], frequency = 7)
#   purchase.model <- stlf(purchase.t, h=10, s.window=3, method = 'ets',ic='bic')
#   redeem.model <- stlf(redeem.t, h=10, s.window=3, method = 'ets',ic='bic')
#   ori.data[, 2] <- purchase.model$fitted
#   ori.data[, 3] <- purchase.model$fitted
  return(ori.data)
}
run.feature.engineering <- function(train.date.range, predict.date.range, train.file = 'data/p2_user_balance_sum.csv' ){
  ori.data <- get.ori.data(train.file) 
  # browser()
  feature.data1 <- feature.enginearing1(train.date.range, predict.date.range, ori.data)
  # browser()
  # feature.data2 <- feature.enginearing2( feature.data1, ori.data )

    # browser()
  # feature.data3 <- feature.enginearing2( feature.data2, ori.data )
    # return( list(train.purchase, train.redeem, pred.purchase, pred.redeem, ori.verify.data) )
  # feature.data2 <- feature.merge( list( feature.data2, feature.data1 ) )
  # browser()c
  return( feature.data1)
}