## Function iterates over the max files and combine the maximums together

## iterates over the grid cells with maximum files and combines them togther for a single dataset
print("Have an error in the meta data I need to fix, short term fix is checking the bbox")
bbox = data.frame(xll = 113.338953078, yll = -43.6345972634, xur = 153.569469029, yur = -10.6681857235)
# can fix this by using grep

# inputs
dx = dy = 1
data_dir = "/Users/saundersk1/Dropbox/Hard Drive/R/2018/RNOAA Data Wrangling/Data"
block_names = c("AM", "AMNC", "WET", "DRY", "SUM", "AUT", "WIN", "SPR")

# get the grid cells
meta_data = readRDS("Data/AS_meta_data.rds")
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

# iterate over the grid cells to reconstruct the observations
temp_get_max_in_grid_cell <- function(i, grid_cells, block_name, dx = 1, dy = 1){

  cell_xll = grid_cells$longitude[i] %>% as.numeric()
  cell_yll = grid_cells$latitude[i] %>% as.numeric()
  max_file = paste(data_dir, "/BLOCK_MAX/", block_name, "_xll_", cell_xll, "_yll_", cell_yll,
                   "_dx_", dx, "_dy_", dy, ".rds", sep ="")

  if(!file.exists(max_file)) return(NULL)

  temp_max_var <- readRDS(max_file)

  return(temp_max_var)

}

num_cells = nrow(grid_cells)
for(block_name in block_names){
  max_var = mclapply(1:num_cells, temp_get_max_in_grid_cell,
         grid_cells = grid_cells,
         block_name = block_name,
         mc.cores = detectCores())
  max_var = do.call(rbind, max_var)
  saveRDS(max_var, paste("Data/", block_name, "_var.rds", sep = ""))
}
