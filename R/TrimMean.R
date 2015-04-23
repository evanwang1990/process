trim.mean <- function(x, na.rm = TRUE, trim = 0.05)
{
  if(!is.numeric(x))
  {
    warning("argument is not numeric: return NA")
    return(NA_real_)
  }
  
  if(na.rm) x <- x[!is.na(x)]
  
  if(!is.numeric(trim) || length(trim) != 1L) stop("'trim' must be numeric of lenght one")
  
  if(trim >= 0.5) stop("'trim' must be smaller than 0.5")
  else
  {
    if(anyNA(x)) return(NA_real_)
    
    K <- floor(trim * length(x))
    if(K == 0) return(mean(x))
    else return(TrimMean(x, 0, length(x) - 1, K));
  } 
}