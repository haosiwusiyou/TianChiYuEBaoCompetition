
run.preprocess <- function() {
  user.balance.ori.path <- paste(data.dir, user.balance.ori, sep = '')
  day.share.ori.path    <- paste(data.dir, day.share.ori, sep = '')
  bank.shibor.ori.path  <-
    paste(data.dir, bank.shibor.ori, sep = '')
  user.balance.mod.path <- paste(data.dir, user.balance.mod, sep = '')
  day.share.mod.path    <- paste(data.dir, day.share.mod, sep = '')
  bank.shibor.mod.path  <-
    paste(data.dir, bank.shibor.mod, sep = '')
  # preprocess for  bank.shibor and day.share data
  # load bank.shibor and day.share data
  bank.interest <- read.csv(bank.shibor.ori.path)
  day.share <- read.csv(day.share.ori.path)
  user.balance <- read.csv( user.balance.ori.path )
  
  # transform time data in bank.shibor and day.share data
  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  bank.interest[,1] <-
    as.Date(as.character(bank.interest[,1]),format = form)
  day.share[,1] <- as.Date(as.character(day.share[,1]),format = form)
  user.balance[, 1] <- as.Date(user.balance[, 1], format = form )
  Sys.setlocale("LC_TIME", lct)
  
  
  begin.Date <- min(min(bank.interest[,1]), min(day.share[,1]))
  end.Date <- max(max(bank.interest[,1]), max(day.share[,1]))
  
  #
  bank.interest.mod <-
    data.frame(report_date = seq(begin.Date, end.Date, by = 1))
  bank.interest.mod[names(bank.interest)[2:ncol(bank.interest)]] <-
    rep(0 , nrow(bank.interest.mod))
  for (i in 1:nrow(bank.interest.mod)) {
    pos.value <-  (bank.interest[, 1] %in% bank.interest.mod[i, 1])
    if (sum(pos.value) == 1) {
      bank.interest.mod[i,] <- bank.interest[pos.value,]
    }else{
      bank.interest.mod[i, 2:ncol(bank.interest)] <-
        bank.interest.mod[i - 1, 2:ncol(bank.interest)]
    }
  }
  
  #
  day.share.mod <-
    data.frame(report_date = seq(begin.Date, end.Date, by = 1))
  day.share.mod[names(day.share)[2:ncol(day.share)]] <-
    rep(0 , nrow(day.share.mod))
  for (i in 1:nrow(day.share.mod)) {
    pos.value <-  (day.share[, 1] %in% day.share.mod[i, 1])
    if (sum(pos.value) == 1) {
      day.share.mod[i,] <- day.share[pos.value,]
    }else{
      day.share.mod[i ,2:ncol(day.share)] <-
        day.share.mod[i - 1, 2:ncol(day.share)]
    }
  }
  
  write.csv(bank.interest.mod ,bank.shibor.mod.path, row.names = F)
  write.csv(day.share.mod ,day.share.mod.path, row.names = F)
  

  # preprocess for
  user.balance <- read.csv(user.balance.ori.path)
  apply( user.balance ,2 ,  na2zero   )
  user.balance.mod <- aggregate( cbind(total_purchase_amt,  total_redeem_amt) ~ report_date ,data = user.balance ,sum)
  write.csv(user.balance.mod ,user.balance.mod.path ,row.names = F)
  
}
