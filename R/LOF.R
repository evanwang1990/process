#parallel local outlier factor

#'@name lof
#'@title An implementation of the LOF algorithm in parallel computing method
#'@description This function obtain local outlier factors using the LOF algorithm. Namely, given a data set it produces a vector of local outlier factors for each case.
#'@param data matrix or a data set that will be internally coerced into a matrix.
#'@param The number of neighbours that will be used in the calculation of the local outlier factors.
#'@param equal.num the max number of neighbours whose distance equals k_distance. `equal.num` = 0, by default
lof <- function(data, k, equal.num = 0)
{
  if(is.data.frame(data)) data <- as.matrix(data)
  if(!is.matrix(data)) stop(substitute(data), " is not a data.frame or a matrix! \n")
  if(mode(data) == "character") stop("Some character variables are in ",substitute(data), "\n")
  
  if(k <= 0 || is.character(k)) stop("k is smaller than 0 or is a character")
  if(k + equal.num > dim(data)[1]) stop("the neighbors are more than the whole points")
  
  res <- parallelLOF(data = data, k = k, equal_num = equal.num)
  res
}