meta_data <-readRDS("Data/AS_meta_data.rds")

dry_threshold = 100 # Remember RNOAA record prcp in decilitres
block_names = c("AM", "AMNC", "WET", "DRY", "SUM", "AUT", "WIN", "SPR") #"AM4"
start_months = c(1, 7, 11, 5, 12, 3, 6, 9) #1
end_months = c(12, 6, 4, 10, 2, 5, 8, 11) # 12

coords <- meta_data %>%
  select(id, longitude, latitude)

len = length(block_names)
result_list = vector("list", len)

for(i in 1:len){

  block_name = block_names[i]
  start_month = start_months[i]
  end_month = end_months[i]

  max_data <- readRDS(paste("Data/", block_name ,"_var.rds", sep = ""))

  warning("max_data has an error where the block is the id")
  spread_max_data = max_data %>%
    filter(id != "block") %>%
    mutate(block = get_block_index(date, start_month, end_month)) %>%
    select(block, id, prcp) %>%
    spread(key = id, value = prcp) %>%
    select(-block)

  cl <- parallel::makeCluster(detectCores())
  doParallel::registerDoParallel(cl)
  location_par <-
    foreach(i = 1:ncol(spread_max_data),
          .combine = 'rbind', .packages = 'RaingleExtremes') %dopar% {
      x = spread_max_data[,i]
      par = fit_fevd_with_tryCatch(x)
      par[1]
    }
  parallel::stopCluster(cl)

  result <- location_par %>%
    as.data.frame() %>%
    mutate(id = names(spread_max_data)) %>%
    mutate(dry = location < dry_threshold) %>%
    left_join(coords, by = "id")

  result_list[[i]] = result

}

ggplot(data = result_list[[8]]) +
  geom_point(aes(x = longitude, y = latitude,
                 col = as.factor(dry)), size = 0.5) +
  scale_color_manual(values = c("orangered", "dodgerblue")) +
  theme_bw()
