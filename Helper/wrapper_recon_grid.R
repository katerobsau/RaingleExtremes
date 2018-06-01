# Have an error in the meta data I need to fix, short term fix is checking the bbox
bbox = data.frame(xll = 113.338953078, yll = -43.6345972634, xur = 153.569469029, yur = -10.6681857235)

# inputs
meta_data = readRDS("Data/AS_meta_data.rds")
data_dir = "/Users/saundersk1/Dropbox/Hard Drive/R/2018/RNOAA Data Wrangling/Data"
# restrictions on our data
min_years = 20
end_year = 2017
start_year= 1910
# figures loosely taken from Vincente
# chose to use spearman instead of pearson
search_radius = 15 # search_radius is 15 km
min_overlap = 200 # need 3 years in common so minimum overlap is
    # 365*0.2*3 #3 years in common, 20% probably of rain on a given day, approx 200
min_corr_value = 0.6 # paper uses 0.5 for correlation, let's be stricter but not much (largely arbitrary)
corr_type = "spearman"

# get the grid cells
meta_xll = meta_data %>% select(longitude) %>% floor
meta_yll = meta_data %>% select(latitude) %>% floor
grid_cells = data.frame(meta_xll, meta_yll) %>%
  distinct() %>%
  # Have an error in the meta data I need to fix, short term fix is checking the bbox
  filter(longitude >= bbox$xll & longitude <= bbox$xur) %>%
  filter(latitude >= bbox$yll & latitude <= bbox$yur)

# order the grid cells
x.seq = seq(min(meta_xll), max(meta_xll))
y.seq = seq(min(meta_yll), max(meta_yll))
all_cells = expand.grid(longitude = x.seq, latitude = y.seq) %>%
  as.data.frame() %>%
  mutate(row_ind = 1:(length(x.seq)*length(y.seq)))
grid_cells = grid_cells %>%
  left_join(all_cells, by = c("longitude", "latitude"))
ord = order(grid_cells$row_ind)
grid_cells = grid_cells[ord, ]
grid_cells <- grid_cells %>% select(-row_ind)

#iterate over the grid cells to reconstruct the observations
plot(grid_cells, pch = 3)
for(i in 1:nrow(grid_cells)){
  cell_xll = grid_cells$longitude[i] %>% as.numeric()
  cell_yll = grid_cells$latitude[i] %>% as.numeric()
  recon_done = recon_grid(meta_data, cell_xll, cell_yll,
                  data_dir = data_dir,
                  search_radius = search_radius,
                  min_overlap = min_overlap,
                  min_corr_value = min_corr_value,
                  corr_type = corr_type,
                  min_years = min_years,
                  end_year = end_year,
                  start_year = start_year)

  points(cell_xll, cell_yll, col = "blue", pch = 3)

}

