#' Get the highest observations in the block
#'
#' @param stn_prcp columns of block, date, prcp
#' @param block_val block to get the maxima from
#' @param rmax number of highest observations to get per block (default = 1)
#'
#' @return Returns the highest observations in the block matching \code{block_val}
#' The returned data frame has columns of block, data and prcp.
#' If insufficient data, then \code{NULL} is returned
#'
#' @export
#'
#' @examples
#' prcp = c(1:365, 2*(1:365))
#' dates = seq(as.Date("1910-01-01"), as.Date("1911-12-31"), by = "days")
#' block = get_block_index(dates, 1, 12)
#' stn_prcp = data.frame(block, date = dates, prcp)
#' get_block_max(stn_prcp, 1910)
#' get_block_max(stn_prcp, 1910, rmax = 4)
#' block = get_block_index(dates, 3, 5)
#' stn_prcp = data.frame(block, date = dates, prcp)
#' get_block_max(stn_prcp, 1910)
#' get_block_max(stn_prcp, 1910, rmax = 4)
get_block_max <- function(stn_prcp, block_val, rmax = 1){

  # check there are sufficient observations
  if(sum(stn_prcp$block == block_val, na.rm = TRUE) < rmax){
    return(NULL)
  }

  # get the block data
  block_prcp <- stn_prcp %>%
    filter(block == block_val)

  # order the precipitation and pull out the highest entries
  ind = order(block_prcp$prcp, decreasing = TRUE)[1:rmax]
  max_prcp = block_prcp[ind, ]

  return(max_prcp)
}
