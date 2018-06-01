# Iterates over the grid cells to get the block maxima
# combines prcp and recon and runs get max code

print("Have an error in the meta data I need to fix, short term fix is checking the bbox")
bbox = data.frame(xll = 113.338953078, yll = -43.6345972634, xur = 153.569469029, yur = -10.6681857235)
# can fix this by using grep

# inputs
meta_data = readRDS("Data/AS_meta_data.rds")
dx = dy = 1
data_dir = "/Users/saundersk1/Dropbox/Hard Drive/R/2018/RNOAA Data Wrangling/Data"
qflags = c(" ", "O")#, "G")
print("Need to check these using the spatial inconsistency check")
block_names = c("AM", "AMNC", "WET", "DRY", "SUM", "AUT", "WIN", "SPR") #"AM4"
start_months = c(1, 7, 11, 5, 12, 3, 6, 9) #1
end_months = c(12, 6, 4, 10, 2, 5, 8, 11) # 12
block_df = data.frame(block_names, start_months, end_months)
end_year = 2017
start_year = 1910
min_years = 20
rmax = 1 #4 # 1

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

  # get the prcp
  prcp_file = paste(data_dir, "/","PRCP_xll_", cell_xll, "_yll_", cell_yll,
              "_dx_", dx, "_dy_", dy, ".rds", sep ="")
  if(!file.exists(prcp_file)) next
  prcp_var = readRDS(prcp_file)
  combine_var <- prcp_var %>%
    mutate(prcp = if_else(qflag_prcp %in% qflags, prcp, NA_real_))

  # get the recon
  recon_file = paste(data_dir, "/RECON/","RECON_xll_", cell_xll, "_yll_", cell_yll,
                   "_dx_", dx, "_dy_", dy, ".rds", sep ="")
  if(file.exists(recon_file)){
    recon_var = readRDS(recon_file)
    if(is.null(recon_var)) next
    if(!all(is.na(recon_var$recon_prcp))){
      # combine prcp and recon
      combine_var <- combine_var %>%
        left_join(recon_var, by = c("id", "date")) %>%
        mutate(prcp = if_else(!is.na(prcp), prcp, recon_prcp))
      }
  }

  # get the stations to take the max of
  cell_meta_data = meta_data %>%
    filter(longitude >= cell_xll & longitude < cell_xll + dx) %>%
    filter(latitude >= cell_yll & latitude < cell_yll + dy) %>%
    mutate(num_years = pmin(last_year, end_year) - pmax(first_year, start_year) + 1) %>%
    filter(num_years >= min_years)
  stn_ids = intersect(cell_meta_data$id, prcp_var$id)
  if(length(stn_ids) == 0) next

  # spread the data
  spread_prcp = combine_var %>%
    filter(id %in% stn_ids) %>%
    select(date, id, prcp) %>%
    spread(key = id, value = prcp)

  # iterate over each block type, get max and write max
  for(b in 1:nrow(block_df)){
    block_name = block_df$block_names[b]
    start_month = block_df$start_months[b]
    end_month = block_df$end_months[b]
    max_file = paste(data_dir, "/BLOCK_MAX/", block_name, "_xll_", cell_xll, "_yll_", cell_yll,
                     "_dx_", dx, "_dy_", dy, ".rds", sep ="")
    if(file.exists(max_file)) next
    temp_spread = spread_prcp %>%
    mutate(block = get_block_index(dates = date, start_month = start_month, end_month = end_month))
    max_data = get_spread_max(prcp_spread = temp_spread,
                            start_month = start_month,
                            end_month = end_month,
                            rmax = rmax)
    saveRDS(max_data, max_file)
  }

  points(cell_xll, cell_yll, col = "red", pch = 3, lwd = 2)

}
