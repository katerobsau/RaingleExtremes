### Development stuff

# utils_spread_accum <- function(date_val, days, mdpr){
#
#     date_spread = seq(date_val - (days - 1), date_val, by = "days")
#
#     total_spread = rep(mdpr/days, each = days)
#
#     output = data.frame(date = date_spread, mdpr_spread = total_spread)
#
#     return(output)
# }
#
# utils_wrapper_spread_accum <- function(dapr_var, mdpr_var){
#
#   print("ADD FILTER FOR QFLAGS FOR DAPR AND MDPR")
#
#   date_accum = dapr_var %>% filter(dapr > 0) %>% select(date)
#
#   days_accum = dapr_var %>% filter(dapr > 0) %>% select(dapr) %>%
#     unlist() %>% as.numeric()
#
#   total_accum = mdpr_var %>% filter(mdpr > 0) %>% select(mdpr) %>%
#     unlist() %>% as.numeric()
#
#   spread_mapply <- mapply(utils_spread_accum,
#                  date_accum$date,
#                  days_accum,
#                  total_accum, SIMPLIFY = TRUE)
#
#   spread_list <- apply(spread_mapply, 2, as.data.frame)
#
#   spread_df <- do.call(rbind, spread_list)
#
#   return(spread_df)
#
# }
#
# utils_combine_prcp_spread_mdpr <- function(prcp_var, spread_mdpr_var){
#
#   combine_prcp <- left_join(prcp_var, spread_mdpr_var, by = "date") %>%
#     mutate(combine = prcp) %>%
#     mutate(combine = ifelse(qflag_prcp == " ", prcp, NA)) %>%
#     mutate(combine = ifelse(!is.na(mdpr_spread),
#                             ifelse(mdpr_spread > 0, mdpr_spread, prcp),
#                             prcp)) %>%
#     select(date, combine)
#
#   return(combine_prcp)
#
# }
#
# utils_get_seasonal_data <- function(combine_prcp, season_start_month, season_end_month){
#
#   if(season_start_month <= season_end_month){
#     season_prcp <- combine_prcp %>%
#       filter(month(date) >= season_start_month & month(date) <= season_end_month)
#   }else{
#     season_prcp <- combine_prcp %>%
#       filter(month(date) >= season_start_month | month(date) <= season_end_month)
#   }
#
#   season_prcp <- season_prcp %>%
#     mutate(block = get_block_index(date, season_start_month, season_end_month)) %>%
#     mutate(day = day(date), month = month(date)) %>%
#     select(-date) %>%
#     spread(key = block, value = combine)
#
#   return(season_prcp)
#
# }
#
# utils_seasonal_mean <- function(spread_df){
#
#   spread_prcp <- spread_df %>% select(-day, -month)
#   col_total = colSums(spread_prcp, na.rm = TRUE)
#   num_rf_obs_per_col = colSums(spread_prcp > 0, na.rm = TRUE)
#   num_obs_per_col = colSums(spread_prcp >= 0, na.rm = TRUE)
#   season_mean <- data.frame(num_rainy = num_rf_obs_per_col,
#                             num_obs = num_obs_per_col,
#                             total = col_total) %>%
#     mutate(block = names(spread_prcp)) %>%
#     mutate(mean = ifelse(!is.na(num_rainy),
#                              ifelse(num_rainy > 0, total/num_rainy, 0),
#                              NA))
#
#   return(season_mean)
#
# }
#
# utils_wrapper_seasonal_mean <- function(prcp_var, dapr_var, mdpr_var){
#
#   spread_mdpr_var <- utils_wrapper_spread_accum(dapr_var, mdpr_var)
#
#   combine_prcp <- utils_combine_prcp_spread_mdpr(prcp_var, spread_mdpr_var)
#
#   austral_summer = utils_get_seasonal_data(combine_prcp, 12, 2)
#   austral_autumn = utils_get_seasonal_data(combine_prcp, 3, 5)
#   austral_winter = utils_get_seasonal_data(combine_prcp, 6, 8)
#   austral_spring = utils_get_seasonal_data(combine_prcp, 9, 11)
#
#   austral_summer_mean = utils_seasonal_mean(austral_summer) %>%
#     mutate(season = "Summer")
#   austral_autumn_mean = utils_seasonal_mean(austral_autumn) %>%
#     mutate(season = "Autumn")
#   austral_winter_mean = utils_seasonal_mean(austral_winter) %>%
#     mutate(season = "Winter")
#   austral_spring_mean = utils_seasonal_mean(austral_spring) %>%
#     mutate(season = "Spring")
#
#   station_season_mean = rbind(
#     austral_summer_mean, austral_autumn_mean,
#     austral_winter_mean, austral_spring_mean
#     )
#
#   return(station_season_mean)
#
# }
