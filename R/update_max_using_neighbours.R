#' Checks if the date of the missing maximum is similar to neighbours.
#' If it is, them it keeps the maxima despite the missing data.
#'
#' @param meta_data meta data of the stations
#' @param stn_id index of the station
#' @param max_var data frame with columns id, date, prcp, block, num_obs, max
#' output from \code{get_stn_max()}. The prcp col is the raw maxima,
#' date is the date of the maxima, num_obs is the number of observations per block,
#' and max is set to NA when there are not enough num_obs observations per block
#' based on the \code{min_perc} set in \code{get_stn_max()}.
#' @param search_radius (default = 20) radius of neighbours to compare dates
#'
#' @return Returns the updated station max object that keeps the missing maxima
#' if the date is common with the neighbours.
#'
#' @examples
#' stn_id = "ASN00040383"
#' nbr_id = "ASN00040225"
#' meta_data = readRDS("Data/AS_meta_data.RDS")
#' stn_dates = c(as.Date("1966-01-21"), as.Date("1967-03-05"), as.Date("1968-02-02"))
#' stn_block = get_block_index(stn_dates, 1, 12)
#' stn_prcp = c(100, 200, 300)
#' stn_max = c(NA, 200, NA)
#' stn_num_obs = c(100, 365, 100)
#' max_var_stn = data.frame(id = rep(stn_id, 3),
#' date = stn_dates, prcp = stn_prcp, block = stn_block, num_obs = stn_num_obs,
#' max = stn_max)
#' nbr_dates = c(as.Date("1966-01-21"), as.Date("1967-03-05"), as.Date("1968-02-02"))
#' nbr_block = get_block_index(nbr_dates, 1, 12)
#' nbr_prcp = c(101, 201, NA)
#' nbr_max = nbr_prcp
#' nbr_num_obs = c(365, 365, 0)
#' max_var_nbr = data.frame(id = rep(nbr_id, 3),
#' date = nbr_dates, prcp = nbr_prcp, block = nbr_block, num_obs = nbr_num_obs,
#' max = nbr_max)
#' max_var = rbind(max_var_stn, max_var_nbr)
#' update_max_using_neighbours(meta_data, stn_id, max_var)
#'
update_max_using_neighbours <- function(meta_data, stn_id, max_var,
                                     search_radius = 20){

  # get station meta data
  stn_meta <- meta_data %>% filter(id == stn_id)

  # get station max
  stn_max <- max_var %>% filter(id == stn_id)
  if(all(is.na(stn_max$max))) return(stn_max)

  # are any max NA but prcp not
  check_prcp <- stn_max %>%
    filter(id == stn_id) %>%
    filter(date >= as.Date(paste(stn_meta$first_year, "01", "01", sep = "-"))) %>%
    filter(date <= as.Date(paste(stn_meta$last_year, "12", "31", sep = "-"))) %>%
    filter(is.na(max) & !is.na(prcp)) %>%
    filter(prcp != 0)
  if(nrow(check_prcp) == 0) return(stn_max)

  # get nearby stations
  nearby_meta_data <- meteo_nearby_stations(lat_lon_df = stn_meta,
                                            station_data = meta_data,
                                            radius = search_radius)[[1]]
  if(length(nearby_meta_data$id) == 1) return(stn_max)

  # get nearby max
  nearby_max <- max_var %>%
    filter(id %in% setdiff(nearby_meta_data$id, stn_id)) %>%
    filter(block %in% check_prcp$block) %>%
    filter(!is.na(max))
  if(nrow(nearby_max) == 0) return(stn_max)

  # check date of prcp with nearby stations
  if(any(check_prcp$date %in% nearby_max$date)){
    update_stn_max <- stn_max %>%
      mutate(max = if_else(date %in% nearby_max$date, prcp, NA_real_))
  }

  return(update_stn_max)

}
