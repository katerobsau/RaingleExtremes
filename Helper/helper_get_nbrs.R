load("Data/AS_meta_data.RData")
search_radius = 50
date_min = "1910-01-01"
year_min = year(date_min)
data_dir = "/Users/saundersk1/Dropbox/Hard Drive/Data/GHCN-Daily/"

### ---------------------------------------------------------------------------

lat_lon_df = as_meta_data %>%
    filter(element == "PRCP") %>%
    select(id, latitude, longitude) %>%
    as.data.frame()

nbr_stns <- rnoaa::meteo_nearby_stations(
    lat_lon_df,
    station_data = as_meta_data,
    var = "PRCP",
    year_min = year_min,
    radius = search_radius)

num_nbrs = lapply(nbr_stns, nrow) %>%
  unlist %>%
  as.vector()
col1 = rep(names(nbr_stns), times = num_nbrs)
col2 = do.call(rbind, nbr_stns)

nbr_df = data.frame(stn1 = col1, stn2 = col2$id, stringsAsFactors = FALSE) %>%
  dplyr::mutate(num1 = stn1 %>% substr(start = 4, stop = 12) %>% as.numeric(),
      num2 = stn2 %>% substr(start = 4, stop = 12) %>% as.numeric()) %>%
  dplyr::filter(stn1 != stn2) %>%
  dplyr::mutate(temp1 = if_else(num1 > num2, stn2, stn1),
                temp2 = if_else(num1 > num2, stn1, stn2)) %>%
  dplyr::mutate(stn1 = temp1, stn2 = temp2) %>%
  dplyr::select(stn1, stn2) %>%
  dplyr::distinct()
  # dplyr::filter(num1 < num2) %>%
  # dplyr::select(-num1, -num2) %>%
  # dplyr::distinct()

file_name = paste(data_dir, "nbrs_radius", search_radius, ".rds", sep = "")
saveRDS(nbr_df, file = file_name)

