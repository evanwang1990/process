recode <- function(var, recodes, deal.time = FALSE, as.factor.result = TRUE, levels){
  exist_package <- search()
  if(!('package:data.table' %in% exist_package)) require(data.table)
  if(!('package:stringr' %in% exist_package)) require(stringr)
  if(!('package:lubridate' %in% exist_package) & deal.time) require(lubridate)
  
  is.fac <- is.factor(var)
  if(is.fac) var <- as.character(var)
  
  recodes <- str_replace_all(recodes, '\n|\t| ', '')
  recode.list <- rev(strsplit(recodes, split = ';')[[1]])
  
  if(deal.time){
    low <- ymd('19000101')
    high <- ymd('30000101')
  }else{
    low <- -Inf
    high <- Inf
  }
  
  valid <- function(string){
    res <- try(eval(parse(text = string)), silent = TRUE)
    if(deal.time){
      if(is.na(res)) stop('\n  ', string, ' is invalid')
    }else{
      if (class(res) == 'try-error') stop('\n  ', string, ' is invalid')
    }
    res
  }
  
  if(deal.time) recode.res <- seq_along(var)  #不可以对时间向量赋值，即x[]<-2错误！
  else recode.res <- var
  
  for(term in recode.list){
    #target <- valid(strsplit(term, '=')[[1]][2])
    target <- strsplit(term, '=')[[1]][2]
    recode <- strsplit(term, '=')[[1]][1]
    if(!is.na(str_extract(recode, '<-<'))){
      lo <- valid(strsplit(recode, '<-<')[[1]][1])
      hi <- valid(strsplit(recode, '<-<')[[1]][2])
      if(lo >= hi) stop(paste('error in', recode))
      recode.res[lo < var & var < hi] <- target
    }else if(!is.na(str_extract(recode, '<-'))){
      lo <- valid(strsplit(recode, '<-')[[1]][1])
      hi <- valid(strsplit(recode, '<-')[[1]][2])
      if(lo >= hi) stop(paste('error in', recode))
      recode.res[lo < var & var <= hi] <- target
    }else if(!is.na(str_extract(recode, '-<'))){
      lo <- valid(strsplit(recode, '-<')[[1]][1])
      hi <- valid(strsplit(recode, '-<')[[1]][2])
      if(lo >= hi) stop(paste('error in', recode))
      recode.res[lo <= var & var < hi] <- target
    }else if(!is.na(str_extract(recode, '--'))){
      lo <- valid(strsplit(recode, '--')[[1]][1])
      hi <- valid(strsplit(recode, '--')[[1]][2])
      if(lo > hi) stop(paste('error in', recode))
      recode.res[lo <= var & var <= hi] <- target
    }else if(!is.na(str_extract(recode, 'else'))){
      recode.res[] <- target
    }else{
      set <-valid(recode)
      recode.res[var %in% set] <- target
    }
  }
 
  if(as.factor.result | is.factor(var)){
    if(missing(levels)){
      recode.res <- factor(recode.res, levels = levels)
    }else{
      recode.res <- factor(recode.res)
    }
  }
  recode.res
}