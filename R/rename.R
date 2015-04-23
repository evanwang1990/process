rename <- function(names)
{
  i <- 1
  names0 <- names
  while(TRUE)
  {
    dup <- duplicated(names)
    names[dup] <- paste0(names0[dup], '_', i)
    if(!any(duplicated(names))) break
    i <- i + 1
  }
  names
}