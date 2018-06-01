#' Wrapper that does the neccessary data wrangling before the missing observations
#' at a station can be reconstructed
#'
#' @param meta_data standard format from rnoaa
#' @param stn_id standard format from GHCN-Daily
#' @param search_radius radius to search for neighbouring ids
#' @param delta_window only reconstruct a missing observation, if other
#' dates in the delta_window have observations
#' @param min_overlap minimum nonzero overlap required to get the correlation between two stations
#' @param from_file (default = TRUE) read in precipitation observations from a file.
#' prcp_var from file must be standard GHCN-Daily format (see rnoaa::meteo_pull_monitors())
#' The form of the file to be read must also take:
#' prcp_file = paste(data_dir, "PRCP_xll_", xll, "_yll_", yll, "_dx_", dx, "_dy_", dy, ".rds", sep ="")
#' @param data_dir if from_file = TRUE, then this is the data_dir where the files are stored
#' @param from_ftp (default = FALSE) read in precipitation observations from the ftp server
#' @param min_corr_value minimum correaltion for a station to be considered a neighbour
#' @param corr_type (default = "spearman") or "pearson"
#' @param prcp_var hard input the precipitation observations from a elsewhere.
#' prcp_var must be standard GHCN-Daily format (see rnoaa::meteo_pull_monitors())
#' @param dx (default 1) is the x grid spacing of the file to be read
#' @param dy (default 1) is the y grid spacing of the file to be read
#' @return Returns a data frame with columns of
#' date, id, recon_prcp and recon_flag corresponding to the prcp at the station
#'
#' @examples
#' stn_id = "ASN00040383"
#' meta_data = readRDS("Data/AS_meta_data.rds")
#' data_dir = "Data"
#' xll = meta_data %>% filter(id == stn_id) %>% select(longitude) %>% floor()
#' yll = meta_data %>% filter(id == stn_id) %>% select(latitude) %>% floor()
#' dx = dy = 1
#' prcp_file = paste(data_dir, "/","PRCP_xll_", xll, "_yll_", yll, "_dx_", dx, "_dy_", dy, ".rds", sep ="")
#' prcp_var = readRDS(prcp_file)
#' stn_prcp = prcp_var %>% filter(id == stn_id)
#' recon_prcp_file <- get_stn_recon(meta_data, stn_id, search_radius = 3, data_dir = data_dir)
#' recon_prcp_ftp <- get_stn_recon(meta_data, stn_id, search_radius = 3, from_file = FALSE, from_ftp = TRUE)
#' #' recon_prcp_ftp <- recon_prcp_ftp %>% filter(date %in% recon_prcp_file$date)
#' recon_prcp_var <- get_stn_recon(meta_data, stn_id, search_radius = 3, from_file = FALSE, from_ftp = FALSE, prcp_var = prcp_var)
#' all(recon_prcp_file == recon_prcp_ftp, na.rm = TRUE)
#' all(recon_prcp_file == recon_prcp_var, na.rm = TRUE)
#' combine_prcp = left_join(stn_prcp, recon_prcp_file, by = c("id", "date"))
#' View(combine_prcp %>% filter(is.na(prcp)))
get_stn_recon <- function(meta_data, stn_id, search_radius = 20,
         from_file = TRUE, data_dir = NULL,
         from_ftp = FALSE,
         delta_window = 30, min_overlap = 3*365,
         min_corr_value = 0.6, corr_type = "spearman",
         prcp_var = NULL, dx = 1, dy = 1){

  #------------------------------------------------------------------------------

  if(!(stn_id %in% meta_data$id)){
    break("Error: station id does not match meta data")
  }

  # get ids of station neighbours
  stn_lat_lon = meta_data %>%
    filter(id == stn_id) %>%
    select(id, latitude, longitude)
  nearby_meta_data <- meteo_nearby_stations(lat_lon_df = stn_lat_lon,
                                            station_data = meta_data,
                                            radius = search_radius)[[1]]

  #------------------------------------------------------------------------------

  # Get the precipitaion variables of all nearby stations

  if(from_file == TRUE){

    xll_vals = floor(nearby_meta_data$longitude)
    yll_vals = floor(nearby_meta_data$latitude)
    distinct_files = data.frame(xll = xll_vals, yll = yll_vals) %>%
      distinct()

    for(i in 1:nrow(distinct_files)){
      xll = distinct_files$xll[i]
      yll = distinct_files$yll[i]
      prcp_file = paste(data_dir, "/","PRCP_xll_", xll, "_yll_", yll,
                        "_dx_", dx, "_dy_", dy, ".rds", sep ="")
      temp_prcp_var = readRDS(prcp_file)
      prcp_var = rbind(prcp_var, temp_prcp_var)
    }

  }else if(from_ftp == TRUE){
    prcp_var <- meteo_pull_monitors(nearby_meta_data$id,
                                    keep_flags = TRUE,
                                    var = "PRCP")
  }else{
    #provided as input
  }

  # Make sure to filter to only get the nearby stations
  prcp_var <- prcp_var %>%
    filter(id %in% nearby_meta_data$id) %>%
    distinct()

  # Update the meta data to include only valid prcp stations
  prcp_meta_data <- nearby_meta_data %>%
    filter(id %in% prcp_var$id)

  # Spread the obs and only keep those of good quality
  prcp_spread = prcp_var %>%
    mutate(prcp_q = if_else(qflag_prcp == " ", prcp, NA_real_)) %>%
    select(-prcp, -mflag_prcp, -sflag_prcp, -qflag_prcp) %>%
    rename(prcp = prcp_q) %>%
    spread(key = id, value = prcp)

  #------------------------------------------------------------------------------

  # Are there neighbours for reconstruction?
  if(nrow(prcp_meta_data) == 1){
    recon_data <- recon_stn(prcp_df = prcp_spread, delta_window)
    return(recon_data)
  }

  #------------------------------------------------------------------------------

  # Which neighbours are correlated
  nbr_ids = setdiff(prcp_meta_data$id, stn_id)
  pair_data = data.frame(p1 = rep(stn_id, length(nbr_ids)),
                         p2 = nbr_ids, stringsAsFactors=FALSE)
  corr_results = apply(pair_data, 1, utils_correlation,
                       type = corr_type,
                       prcp_df = prcp_spread %>% select(-date),
                       min_overlap)

  # Get compatible stations
  corr_pairs = pair_data %>%
    mutate(corr = corr_results) %>%
    filter(corr >= min_corr_value) %>%
    rename(id = p2) %>%
    select(-p1)

  # Get suitable stations
  suitable_stns = corr_pairs %>%
    dplyr::select(id) %>%
    unlist() %>%
    as.vector()

  #------------------------------------------------------------------------------

  # sort the columns of prcp_spread by the correlated neighbours
  if(length(suitable_stns) == 0){
    prcp_spread <- prcp_spread %>% select(date, stn_id)
  }else{
    ord = order(corr_pairs$corr, decreasing = TRUE)
    sorted_suitable_stns = corr_pairs$id[ord]
    prcp_spread <- prcp_spread %>%
      dplyr::select(date, stn_id, sorted_suitable_stns)
  }

  #------------------------------------------------------------------------------

  # reconstruct the station data
  recon_data <- recon_stn(prcp_df = prcp_spread, delta_window)
  return(recon_data)

}
