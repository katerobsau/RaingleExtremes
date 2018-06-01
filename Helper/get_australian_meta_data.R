country_id = "AS"
network_id = "N"
print("Warning: This can take a little while")
station_info = ghcnd_stations()
country_station_info = station_info %>%
  filter(substr(id, 1, 2) == country_id &
           substr(id, 3, 3) == network_id)

saveRDS(country_station_info, "Data/AS_meta_data.rds")
