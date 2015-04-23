#'@name round
#'@title the improved version of base::round function
#'@description get the number closest to its first argument which can be divided by exactly unit
#'@param x numeric vectors which will be rounded
#'@param unit the unit which is smaller than x
#'@examples
#'round(12345, 100)
#'round(1.098, 0.03)
#'@export
round <- function(x, unit)
{
  if(!is.numeric(x)) stop("input vector is not numeric!")
  
  if(unit <= 0)
  {
    return(x)
    warning("the unit is not more than zero, the original values will be returned!")
  }
  
  lower <- floor(x / unit) * unit
  higher <- ceiling(x / unit) * unit
  res <- ifelse(2 * x - lower - higher <= 0, lower, higher)
  res
}
