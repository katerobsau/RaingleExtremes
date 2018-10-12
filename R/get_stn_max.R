#' Get the highest observations in the each block
#'
#' @param stn_prcp columns of block, date, prcp
#' @param stn_id (optional) convienient for indexing
#' @param start_month beginning of the block.
#' @param end_month end of the block.
#' @param min_perc minimum percentage of missing data per block (default = 0.95)
#' @param rmax number of highest observations to get per block (default = 1)
#' @param dates (optional instead of stn_prcp)
#' vector of dates
#' @param prcp (optional instead of stn_prcp)
#' vector of precipitation
#'
#' @return Returns the a data frame with columns id, block, date, prcp, num_obs, max
#' the prcp is the raw maxima, date is the date of the maxima,
#' num_obs is the number of observations per block, and
#' max is set to \code{NA} when there are not enough num_obs
#' observations per block based on the \code{min_perc}
#'
#' @export
#'
#' @examples
#' prcp = c(1:365, 2*(1:365))
#' dates = seq(as.Date("1910-01-01"), as.Date("1911-12-31"), by = "days")
#' block = get_block_index(dates, 1, 12)
#' stn_prcp = data.frame(block, date = dates, prcp)
#' get_stn_max(stn_prcp, "STN_ID", 1, 12)
#' get_stn_max(stn_prcp, "STN_ID", 1, 12, rmax = 2)
#' block = get_block_index(dates, 3, 5)
#' stn_prcp = data.frame(block, date = dates, prcp)
#' get_stn_max(stn_prcp, "STN_ID", 3, 5)
#' get_stn_max(stn_prcp, "STN_ID", 3, 5, rmax = 2)
#' block = get_block_index(dates, 12, 2)
#' stn_prcp = data.frame(block, date = dates, prcp)
#' get_stn_max(stn_prcp, "STN_ID", 12, 2)
#' get_stn_max(stn_prcp, "STN_ID", 12, 2, min_perc = 0.5)
#'
get_stn_max <- function(stn_prcp, stn_id,
                        start_month, end_month,
                        min_perc = 0.95,
                        rmax = 1,
                        dates = NULL, prcp = NULL){

  # Check the input type
  if(missing(stn_prcp)){
    stn_prcp = data.frame(date = dates, prcp = prcp)
  }
  if(missing(stn_id)){
    stn_id = "NA"
  }

  # should be if(!('block' %in% names(stn_prcp)%)) do this
  # Add a new column for the blocks
  block <- get_block_index(stn_prcp$date, start_month, end_month)
  if(all(is.na(block))){
    # no suitable data
    return(NULL)
  }
  stn_prcp <- stn_prcp %>%
    mutate(block = block) %>%
    filter(!is.na(block))

  # Get the max data per block
  block_df <- data.frame(block = unique(block))
  rmax_df = NULL
  for(block_val in unique(block)){
    rmax_df = rbind(rmax_df, get_block_max(stn_prcp, block_val, rmax))
  }
  #Note: This is written as for loop, because
  # sapply converted my dates to numerics ???!!

  # Count the observations in each block
  count_obs <- stn_prcp %>%
    count(block = block*(prcp >= 0)) %>%
    mutate(num_obs = n) %>%
    select(-n)

  # Only keep observations if there are enough per block
  block_len <- get_block_length(start_month, end_month)
  min_obs_per_block <- block_len*min_perc
  max_df <- full_join(rmax_df, count_obs, by = "block") %>%
    filter(!is.na(block)) %>%
    mutate(max = prcp*(num_obs >= min_obs_per_block)) %>%
    mutate(max = replace(max, which(num_obs < min_obs_per_block), NA))
  max_df = cbind(id = rep(stn_id, nrow(max_df)), max_df)

  return(max_df)

}
