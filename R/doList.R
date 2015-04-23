#'@name rbindList
#'@title combine R objects by rows
#'@description improved version of rbind
#'@param objs character vector, the names of dataframes
#'@examples
#'data(iris)
#'x1 <- iris
#'x2 <- iris
#'x3 <- iris
#'rbindList(c('x1', 'x2', 'x3'))
#'@export
rbindList <- function(objs)
{
  if(!is.character(objs)) stop("The input should be a vector of R objects' names\n")
  
  text <- paste0('rbind(', paste0('get("',objs, '")', collapse = ', '), ')')
  expr <- parse(text = text)
  res <- eval(expr)
  res
}

#'@name cbindList
#'@title cbind a set of dataframes
#'@description improved version of rbind, it can rbind a set of dataframes which have the same colomns
#'@param objs character vector, the names of dataframes
#'@param dup.rm if `TRUE`, the variables with the same name will be delete and just the most left side variable will be retained\n
#'if `FALSE`, all the variables will be retained but the variables with the same name will be renamed
#'@examples
#'data(iris)
#'x1 <- iris
#'x2 <- iris
#'x3 <- iris
#'names(cbindList(c('x1', 'x2', 'x3'), dup.rm = T))
#'names(cbindList(c('x1', 'x2', 'x3'), dup.rm = F))
#'@export
cbindList <- function(objs, dup.rm = T)
{
  if(!is.character(objs)) stop("The input should be a vector of R objects' names\n")
  
  text <- paste0('cbind(', paste0('get("',objs, '")', collapse = ', '), ')')
  expr <- parse(text = text)
  res <- eval(expr)
  var_names <- names(res)
  
  if(dup.rm){
    if(is.data.table(res)) res <- res[, which(duplicated(var_names) == F), with = F]
    else if(is.data.frame(res)) res <- res[!duplicated(var_names)]
  }else{
    names(res) <- rename(var_names)
  }
  
  res
}


#'@name mergeList
#'@title merge a set of dataframes
#'@description an improved version of `merge`, which can merge a set of dataframes\n it can rename the varaibles with same names automatically except 'by' variables
#'@param objs character vector, names of dataframes
#'@param by by variable
#'@param all,all.x,all.y the same with merge's
#'@examples
#'data(iris)
#'iris <- cbind(id = 1:20, iris[1:20, ])
#'x1 <- iris
#'x2 <- iris
#'x3 <- iris
#'x4 <- iris
#'mergeList(c('x1', 'x2', 'x3', 'x4'), by = 'id')
#'@export
mergeList <- function(objs, by = NULL, all = FALSE, all.x = FALSE, all.y = FALSE)
{
  if(!is.character(objs)) stop("The input should be a vector of R objects' names\n")
  
  pos <- apply(array(objs), 1, exists)
  if(!all(pos)) stop(paste0(objs[!pos], collapse = ' '), ' not exist!\n')
  
  e <- new.env()
  assign('i', 0, e) #the counter variable in a new environment which is cumulative!! 
  
  res <- Reduce(function(x, y)
  {
    e$i <- e$i + 1
    cat(e$i, '\n')
    if(is.character(x)) x <- get(x)
    if(is.character(y)) y <- get(y)
    names.x <- names(x)
    names.y <- names(y)
    names(y)[names.y %chin% names.x & ! names.y %chin% by] <- paste0(names.y[names.y %chin% names.x & ! names.y %chin% by], '_', e$i) #the variables with same names but not 'by' variables will be renamed
    res <- merge(x, y, by = by, all = all, all.x = all.x, all.y = all.y, )
    res
  }, objs)
  res
}
