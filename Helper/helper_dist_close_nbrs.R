load("Data/AS_meta_data.RData")
data_dir = "/Users/saundersk1/Dropbox/Hard Drive/Data/GHCN-Daily/"
nbr_file = 'nbrs_radius50.rds'

#-----------------------------------------------------------------

# make data into a form where we can use rnoaa::meteo_spherical_distance()
# decided to add this code in so I could speed up some later code

nbr_df = readRDS(paste(data_dir, 'nbrs_radius50.rds', sep = ""))

print("Want to rewrite this using right_join")

reduced_meta_data = as_meta_data %>%
  dplyr::filter(element == "PRCP") %>%
  dplyr::select(id, longitude, latitude) %>%
  dplyr::mutate(stn1 = id) %>%
  dplyr::select(-id)

nbr_dist_df = nbr_df %>%
  dplyr::left_join(reduced_meta_data, by = "stn1")

reduced_meta_data = reduced_meta_data %>%
  dplyr::mutate(stn2 = stn1) %>%
  dplyr::select(-stn1)

nbr_dist_df = nbr_dist_df %>%
  dplyr::left_join(reduced_meta_data, by = "stn2")

nbr_dist_df = nbr_dist_df %>%
  dplyr::select(lat1 = latitude.x, long1 = longitude.x,
                lat2 = latitude.y, long2 = longitude.y)

# ------------------------------------------------------------

print("Want to rewrite this using mapply")
temp_function <- function(row){
  lat1 = row[1] %>% as.numeric()
  long1 = row[2] %>% as.numeric()
  lat2  = row[3] %>% as.numeric()
  long2 = row[4] %>% as.numeric()
  dist_val = rnoaa::meteo_spherical_distance(lat1 = lat1, long1 = long1,
                                             lat2 = lat2, long2 = long2)
  return(dist_val)
}

dist_values = apply(nbr_dist_df, 1, temp_function)

# ------------------------------------------------------------

nbr_df <- nbr_df %>%
  dplyr::mutate(km = dist_values)

file_name = paste(data_dir, "close_nbrs_dist.rds", sep = "")
saveRDS(nbr_df, file = file_name)

# ------------------------------------------------------------
