library(dplyr)
library(parallel)
library(tidyr)
library(foreach)
library(lubridate)

print("COMMENTED OUT THE RECON MAX - MOVE TO ANOTHER FILE")
wd = getwd()
local_dir ="/Users/saundersk1/Documents/Git/RaingleExtremes"
server_dir = "/home/student.unimelb.edu.au/saundersk1/DataWrangle/"

if(wd == local_dir){
  data_dir = "/Users/saundersk1/Dropbox/Hard Drive/Data/GHCN-Daily/"
  as_meta_data_file_name = paste(wd, "/Data/as_meta_data.RData", sep = "")
  load(as_meta_data_file_name)
}else{
  data_dir = paste(wd, "GHCNDaily/", sep = '/')
  as_meta_data_file_name =  paste(data_dir, "AS_meta_data.RData", sep = "")
  load(as_meta_data_file_name)
  source(paste(wd, "get_block_length.R", sep = "/"))
  source(paste(wd, "get_block_index.R", sep = "/"))
  source(paste(wd, "get_block_max.R", sep = "/"))
  source(paste(wd, "spread_get_max.R", sep = "/"))
  source(paste(wd, "get_stn_max.R", sep = "/"))
}

use_recon = FALSE

block_names = c("AM","SUM", "AUT", "WIN", "SPR") #"AM4"
start_months = c(7, 12, 3, 6, 9) #1
end_months = c(6, 2, 5, 8, 11) # 12
block_df = data.frame(block_names, start_months, end_months)

list_files = list.files(path = data_dir)
prcp_files = list_files[stringr::str_detect(list_files, pattern = "PRCP")]
num_files = length(prcp_files)

for(i in 1:num_files){

  print(paste(i, "in", num_files))

  # read in the precipitation data
  prcp_file = paste(data_dir, prcp_files[i], sep = "")
  prcp_parts = strsplit(prcp_files[i], split = "_")[[1]]
  tail_str = paste(prcp_parts[-1], collapse = "_")
  prcp_var = readRDS(prcp_file)
  print(prcp_file)
  if(is.null(prcp_var)) next
  if(nrow(prcp_var) == 0) next

  # spread the original prcp data
  # don't want to take the max if it was flagged for bad quality
  spread_prcp = prcp_var %>%
    dplyr::mutate(prcp = if_else(qflag_prcp %in% c(" ", "O"), prcp, NA_real_)) %>%
    dplyr::select(date, id, prcp) %>%
    tidyr::spread(key = id, value = prcp)

  # iterate over each block type, get max and write max
  for(b in 1:nrow(block_df)){

    block_name = block_df$block_names[b]
    start_month = block_df$start_months[b]
    end_month = block_df$end_months[b]

    # Get the raw maxima
    max_data = get_spread_max(prcp_spread = spread_prcp,
                              start_month = start_month,
                              end_month = end_month,
                              rmax = 1)

    # Save the maxima
    if(is.null(max_data)) next
    if(nrow(max_data) == 0) next
    raw_max_file = paste(data_dir, "RAW_MAX/", block_name, '_', tail_str, sep ="")
    qflag_info <- prcp_var %>%
      dplyr::select(id, date, qflag_prcp)
    max_data = max_data %>%
      dplyr::left_join(qflag_info, by = c("date", "id"))
    saveRDS(max_data, raw_max_file)

    # get the top 4 raw maxima
    if(block_name == "AM"){

      max4_data = get_spread_max(prcp_spread = spread_prcp,
                                 start_month = start_month,
                                 end_month = end_month,
                                 rmax = 4)
      max4_data = max4_data %>%
        dplyr::left_join(qflag_info, by = c("date", "id"))
      raw_max4_file = paste(data_dir, "RAW_MAX4/", block_name, tail_str, sep ="")
      saveRDS(max4_data, raw_max4_file)

    }

  #   if(use_recon == TRUE){
  #
  #   # read in the reconstructed data
  #   recon_file = paste(data_dir, 'RECON', "_", file_str, sep = "")
  #   print(recon_file)
  #   recon_var = readRDS(recon_file)
  #
  #   # merge recon prcp and prcp
  #   if(any(!is.na(recon_var$recon_flag))){
  #     recon_var <- recon_var %>%
  #       dplyr::mutate(recon_prcp = if_else(!is.na(recon_prcp), recon_prcp, prcp))
  #   }else if(all(is.na(recon_var$recon_flag))){
  #     recon_var <- recon_var %>%
  #       dplyr::mutate(recon_prcp = prcp) # nothing got reconstructed handles this case
  #   }
  #
  #   # spread the reconstructed prcp data
  #   spread_recon = recon_var %>%
  #     dplyr::mutate(recon_prcp =
  #                   if_else(qflag_prcp %in% c(" ", "O"), recon_prcp, NA_real_)) %>%
  #     dplyr::mutate(date_lead = lead(date)) %>% # HACKED THIS LINE IN TO DEAL WITH THE ODDNESS OF MY DATA READ
  #     dplyr::filter(date != date_lead | is.na(date_lead)) %>% # HACKED THIS LINE IN TO DEAL WITH THE ODDNESS OF MY DATA READ
  #     dplyr::select(date, id, recon_prcp) %>%
  #     tidyr::spread(key = id, value = recon_prcp)
  #
  #   # get the reconstructured maxima
  #   recon_max_file = paste(data_dir, "RCON_MAX/", block_name, file_str, sep ="")
  #
  #   recon_max_data = get_spread_max(prcp_spread = spread_recon,
  #                             start_month = start_month,
  #                             end_month = end_month,
  #                             rmax = 1)
  #
  #   recon_max_data = recon_max_data %>%
  #     dplyr::left_join(recon_var %>%
  #                        dplyr::select(id, date, qflag_prcp, recon_flag),
  #                      by = c("date", "id"))
  #   saveRDS(recon_max_data, recon_max_file)
  #
  }
}
