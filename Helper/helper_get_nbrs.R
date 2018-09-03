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

num_nbrs = lapply(nbr_stns, nrow) %>% unlist %>% as.vector()
col1 = rep(names(nbr_stns), times = num_nbrs)
col2 = do.call(rbind, nbr_stns)
nbr_df = data.frame(stn = col1, nbr = col2$id, stringsAsFactors = FALSE) %>%
  dplyr::filter(stn != nbr)

file_name = paste(data_dir, "nbrs_radius", search_radius, ".rds", sep = "")
saveRDS(nbr_df, file = file_name)

