duplicate <- function(data, by)
{
  if (!('package::data.table' %in% search())) require(data.table)
  if (!is.data.table(data)) setDT(data)
  if (is.integer(by)){
    if (any(by > nrow(data)) | any(by < 1)) stop('Integer indexes are out of range\n',"It's often better to use colnames")
    else by <- names(data)[by]
  }else{
    bad_cols <- setdiff(by, names(data))
    if (length(bad_cols) > 0) stop(paste0(bad_cols, collapse = ','),'do not exist!')
  }
  dup.rows <- unique(data[duplicated(data, by = by), by, with = F], by = by)
  res <- merge(data, dup.rows, by = by, all = F)  
  res
}
