#'@name duplicate
#'@title Determine Duplicate Rows
#'@description An improved version of `data.table::duplicated`. It returns all the rows which have duplicate rows, which is the difference from `duplicated`
#'@param data data.frame or data.table
#'@param by character vector, `by` variables' names
#'@examples
#'data(iris)
#'duplicate(iris, c('Sepal.Width', 'Species'))
#'@export
duplicate <- function(data, by)
{
  if(!is.data.frame(data)) stop(substitute(data), ' is not data.frame!\n')
  if (!is.data.table(data)) setDT(data)
  if (is.integer(by)){
    if (any(by > nrow(data)) | any(by < 1)) stop('Integer indexes are out of range\n',"It's often better to use colnames")
    else by <- names(data)[by]
  }else{
    bad_cols <- setdiff(by, names(data))
    if (length(bad_cols) > 0) stop(paste0(bad_cols, collapse = ','),'do not exist!')
  }
  
  by <- unique(by)
  setkeyv(data, by)
  dup.rows <- unique(data[duplicated(data, by = by), by, with = F], by = by)
  data[dup.rows, nomatch = 0]
  #merge(data, dup.rows, by = by, all = F)
  #a little slower
}