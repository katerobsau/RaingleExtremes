### Save my data
date_min = "1910-01-01"
date_max = "2017-12-31"
dx = dy = 1
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
             latitude >= yll & latitude < yll + dy)

  if(nrow(stns_in_cell) == 0) next

  stn_ids = stns_in_cell %>%
    dplyr::select(id) %>%
    unlist() %>%
    as.vector()

  file_str = paste("_xll_", xll, "_yll_", yll, "_dx_", dx, "_dy_", dy, ".rds", sep ="")
  wrapper_save_prcp_data(stn_ids = stn_ids, data_dir = data_dir, file_str = file_str,
                 date_min = date_min, date_max = date_max)

}
