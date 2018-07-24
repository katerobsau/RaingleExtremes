#' Get index of the block associated with a given date
#'
#' @param dates vector of dates
#' @param start_month beginning of the block
#' @param end_month end of the block
#' @return Returns the block index for the given date.
#' The block index is a year, and is the year of the \code{start_month}.
#' @export
#'
#' @examples
#' get_block_index(as.Date("1910-01-01"), 1, 12)
#' get_block_index(as.Date("1910-01-01"), 12, 2)
#'
get_block_index <- function(dates, start_month, end_month){
  month = month(dates)
  year = year(dates)
  if(start_month <= end_month){
    block = year*(month >= start_month & month <= end_month)
  }else{
    block = year*(month <= end_month | month >= start_month) -
      (month <= end_month)
  }
  block[which(block == 0)] = NA
  return(block)
}
