#' Essentially a wrapper function to get the maxima
#' at a set of stations at once
#'
#' @param prcp_spread columns of date and station ids
#' @param start_month beginning of the block.
#' @param end_month end of the block.
#' @param min_perc minimum percentage of missing data per block (default = 0.95)
#' @param rmax number of highest observations to get per block (default = 1)
#'
#' @return Returns the a data frame with columns id, block, date, prcp, nonzero, max
#' the prcp is the raw maxima, date is the date of the maxima,
#' nonzero is the number of nonzero observations per block, and
#' max is set to \code{NA} when there are not enough nonzero
#' observations per block based on the \code{min_perc}
#'
#' @export
#'
#' @examples
#' prcp1 = c(1:365, 2*(1:365))
#' prcp2 = c(seq(0.1,36.5, by = 0.1), seq(0.01,3.65, by = 0.01))
#' dates = seq(as.Date("1910-01-01"), as.Date("1911-12-31"), by = "days")
#' prcp_spread = data.frame(date = dates, id1 = prcp1, id2 = prcp2)
#' get_spread_max(prcp_spread, 1, 12)
#'
get_spread_max <- function(prcp_spread, start_month, end_month,
                           rmax = 1, min_perc = 0.95){

  stn_ids <- setdiff(names(prcp_spread), "date")
  max_df = NULL
  for(stn_id in stn_ids){
    stn_prcp = prcp_spread %>% select(date, prcp = stn_id)
    temp_max_df <- get_stn_max(stn_prcp, stn_id,
                               start_month, end_month,
                               min_perc = min_perc,
                               rmax = rmax)
    max_df <- rbind(max_df, temp_max_df)
  }

  return(max_df)
}
