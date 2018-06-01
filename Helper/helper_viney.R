# Have an error in the meta data I need to fix, short term fix is checking the bbox
bbox = data.frame(xll = 113.338953078, yll = -43.6345972634, xur = 153.569469029, yur = -10.6681857235)

# inputs
meta_data = readRDS("Data/AS_meta_data.rds")
data_dir = "/Users/saundersk1/Dropbox/Hard Drive/R/2018/RNOAA Data Wrangling/Data"
# restrictions on our data
min_years = 20
end_year = 2017
start_year= 1910
dx = dy = 1
block_name = "AM"

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

plot(grid_cells, pch = 3)
for(i in 1:nrow(grid_cells)){

  cell_xll = grid_cells$longitude[i]
  cell_yll = grid_cells$latitude[i]
  points(grid_cells[i, ], pch = 3, col = "red")
  # get max
  max_file = paste(data_dir, "/BLOCK_MAX/", block_name, "_xll_", cell_xll, "_yll_", cell_yll,
                 "_dx_", dx, "_dy_", dy, ".rds", sep ="")
  if(!file.exists(max_file)){
    next
  }else{
    max_var <- readRDS(max_file)
  }

  # get prcp
  prcp_file = paste(data_dir, "/PRCP", "_xll_", cell_xll, "_yll_", cell_yll,
                   "_dx_", dx, "_dy_", dy, ".rds", sep ="")
  prcp_var = readRDS(prcp_file)

  # get dapr
  dapr_file = paste(data_dir, "/DAPR", "_xll_", cell_xll, "_yll_", cell_yll,
                    "_dx_", dx, "_dy_", dy, ".rds", sep ="")
  if(!file.exists(dapr_file)){
    dapr_var = NULL
  }else{
    dapr_var <- readRDS(dapr_file)
  }

  stn_ids = lapply(max_var %>% select(id) %>% distinct(), as.character) %>%
    unlist() %>% as.vector()
  stn_ids = setdiff(stn_ids, "block")

  viney_result = wrapper_viney(stn_ids = stn_ids,
                               prcp_var = prcp_var,
                               dapr_var = dapr_var)

  viney_file = paste(data_dir, "/VINEY", "_xll_", cell_xll, "_yll_", cell_yll,
  "_dx_", dx, "_dy_", dy, ".rds", sep ="")

  viney_ouptut = filter(viney_result, p_value < 0.0008)

}

# failed_years = filter(viney_result, p_value < 0.0008)
# ids = lapply(failed_years %>% select(id) %>% distinct(), as.character) %>%
#   unlist() %>% as.vector()
# ggplot(data = viney_result %>% filter(id %in% ids), aes(x = block, y = p_value, col = id)) +
#   geom_line() +
#   scale_y_log10() +
#   geom_hline(yintercept = 0.0008, linetype = "dashed") +
#   geom_hline(yintercept = 0.5, linetype = "dashed")
#
# ggplot(data = viney_result %>% filter(!(id %in% ids)), aes(x = block, y = p_value, col = id)) +
#   geom_line() +
#   scale_y_log10() +
#   geom_hline(yintercept = 0.0008, linetype = "dashed") +
#   geom_hline(yintercept = 0.5, linetype = "dashed")
