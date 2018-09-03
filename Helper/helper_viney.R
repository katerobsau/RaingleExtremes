### Save my data
date_min = "1910-01-01"
date_max = "2017-12-31"
dx = dy = 1
min_years = 20
data_dir = "/Users/saundersk1/Dropbox/Hard Drive/Data/GHCN-Daily/"

load("Data/as_meta_data.RData")

long_seq = seq(as_meta_data$longitude %>% min() %>% floor(),
               as_meta_data$longitude %>% max() %>% ceiling(),
               by = dx)

lat_seq = seq(as_meta_data$latitude %>% min() %>% floor(),
              as_meta_data$latitude %>% max() %>% ceiling(),
              by = dy)

grd = expand.grid(longitude = long_seq, latitude = lat_seq) %>%
  as.data.frame()

plot(grd$longitude, grd$latitude, pch = 3)
for(i in 1:nrow(grd)){

  xll = grd$longitude[i]
  yll = grd$latitude[i]
  points(xll, yll, pch = 3, col = "red")

  stns_in_cell = as_meta_data %>%
    dplyr::filter(element == "PRCP") %>%
    dplyr::filter(longitude >= xll & longitude < xll + dx &
                    latitude >= yll & latitude < yll + dy) %>%
    dplyr::filter(last_year - first_year + 1 >= min_years)

  if(nrow(stns_in_cell) == 0) next

  stn_ids = stns_in_cell %>%
    dplyr::select(id) %>%
    unlist() %>%
    as.vector()

  print("GOTTA FIX THIS FILE STRING")
  file_str = paste("_xll_", xll, "_yll_", yll, "_dx_", dx, "_dy_", dy, ".rds.rds", sep ="")
  prcp_var = readRDS(paste(data_dir, "PRCP", file_str, sep = ""))
  dapr_var = readRDS(paste(data_dir, "DAPR", file_str, sep = ""))

  print("GOTTA MOVE VINEY")
  viney_result = wrapper_viney(stn_ids = stn_ids,
                               prcp_var = prcp_var,
                               dapr_var = dapr_var)

  file_str = paste("_xll_", xll, "_yll_", yll, "_dx_", dx, "_dy_", dy, ".rds", sep ="")
  viney_file = paste(data_dir, "VINEY", file_str, sep ="")

  saveRDS(viney_result, file = viney_file)

}
