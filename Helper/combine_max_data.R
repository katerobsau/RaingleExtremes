### GOT BITS HACKED IN EVERYWHERE

## iterates over the grid cells with maximum files and combines them togther for a single dataset

print("Have an error in the meta data I need to fix, short term fix is checking the bbox")
bbox = data.frame(xll = 113.338953078, yll = -43.6345972634, xur = 153.569469029, yur = -10.6681857235)
# can fix this by using grep

# inputs
meta_data = readRDS("Data/AS_meta_data.rds")
dx = dy = 1
data_dir = "/Users/saundersk1/Dropbox/Hard Drive/R/2018/RNOAA Data Wrangling/Data"
block_name = "AMNC"

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

# iterate over the grid cells to reconstruct the observations
plot(grid_cells, pch = 3)
max_var = NULL
for(i in 1:nrow(grid_cells)){

  cell_xll = grid_cells$longitude[i] %>% as.numeric()
  cell_yll = grid_cells$latitude[i] %>% as.numeric()

  max_file = paste(data_dir, "/BLOCK_MAX/", block_name, "_xll_", cell_xll, "_yll_", cell_yll,
                   "_dx_", dx, "_dy_", dy, ".rds", sep ="")
  if(!file.exists(max_file)) next
  temp_max_var <- readRDS(max_file)
  max_var <- rbind(max_var, temp_max_var)
  points(cell_xll, cell_yll, col = "blue", pch = 3)
}

saveRDS(max_var, paste("Data/", block_name, "_var.rds", sep = ""))

#### ----------------------------------------------

# max data conditions

# identify which of these were reconstructed (really should save this out earlier)

# run king tests
min_num_dates = 80
max4_var = readRDS("Data/AM4_var.rds")
stn_ids = unique(max4_var$id)
len = length(stn_ids)
king_results = vector("list", len)
names(king_results) = stn_ids
plot(meta_data$longitude, meta_data$latitude,
     pch = 3, cex = 0.2,
     xlim = c(bbox$xll, bbox$xur),
     ylim = c(bbox$yll, bbox$yur))
for(i in 1:len){
  stn_id = stn_ids[i]
  stn_dates <- max4_var %>%
      filter(id == stn_id) %>%
      filter(!is.na(max)) %>%
      select(date)
  stn_dates = stn_dates[,1]
  freq_results = extremes_untagged_test(stn_dates,
                                        min_obs = min_num_dates)
  king_results[[i]] = freq_results
  points(
    meta_data %>% filter(id == stn_id) %>% select(longitude),
    meta_data %>% filter(id == stn_id) %>% select(latitude),
    col = "orange", pch = 3, cex = 0.2)
}
saveRDS(king_results, "Data/king_results.rds")

# run viney bates tests
temp_fun <- function(l){
  if(length(l) == 1){
    return(NA)
  }else if(any(l$bool == TRUE)){
   return("FAIL")
  }else{
   return("PASS")
  }
}
king_test = lapply(king_results, temp_fun)

# add qflags...

# missing data restriction

