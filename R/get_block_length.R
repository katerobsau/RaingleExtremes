#' Get the length of the block
#'
#' @param start_month beginning of the block.
#' @param end_month end of the block.
#' @return The number of days between the \code{start_month} and the \code{end_month}. Function ignores February 29.
#'
#' @export
#'
#' @examples
#' get_block_length(1, 12)
#' get_block_length(12, 2)

get_block_length <- function(start_month, end_month){
  month_vec = month(seq(as.Date("1910-01-01"),
                        as.Date("1910-12-31"), by = "days"))
  if(start_month <= end_month){
    block_len = sum(month_vec >= start_month & month_vec <= end_month)
  }else{
    block_len = sum(month_vec >= start_month | month_vec <= end_month)
  }
  return(block_len)
}

