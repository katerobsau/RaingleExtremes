library(dplyr)
library(parallel)
library(tidyr)
library(foreach)
library(lubridate)

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
}

crit_viney_pval = 0.008
max_data_dir =paste(data_dir, "RCON_MAX/", sep = "")
list_files = list.files(path = max_data_dir)
block_patterns = c("AM", "SPR", "WIN", "SUM", "AUT")
for(block_pattern in block_patterns){

  block_pattern = paste(block_pattern, "xll",sep = "_")
  max_files = list_files[stringr::str_detect(list_files, pattern = block_pattern)]
  num_files = length(max_files)

for(i in 1:num_files){

  print(paste(block_pattern, i, "in", num_files))

  # read in our max data
  max_file = paste(max_data_dir, max_files[i], sep = "")
  print(max_file)
  max_var = readRDS(max_file)

  # run viney test (helper_viney.R)
  viney_file = paste(c("VINEY", strsplit(max_files[i], split = "_")[[1]][-1]), collapse = "_")
  viney_filepath = paste(data_dir, viney_file, sep  = '')
  viney_result = readRDS(viney_filepath)

  # update max file path
  max_parts = strsplit(max_files[i], split = "_")[[1]]
  max_start = paste(max_parts[1], "VINEY", sep='_')
  new_max_file = paste(c(max_start, max_parts[-1]), collapse = "_")
  new_max_filepath = paste(max_data_dir, new_max_file, sep  = '')
  print(new_max_filepath)

  # check if we need to worry
  if(is.null(viney_result)){
    saveRDS(max_var, new_max_filepath)
    next
  }

  # filter by suspect pvalues
  viney_result <- viney_result %>%
    dplyr::filter(p_value < crit_viney_pval)

  # check again if we need to do any data handling
  if(nrow(viney_result) == 0){
    saveRDS(max_var, new_max_filepath)
    next
  }
  viney_ids = unique(viney_result$id)

  # read in the prcp data
  prcp_file = paste(c("PRCP", strsplit(max_files[i], split = "_")[[1]][-1]), collapse = "_")
  prcp_filepath = paste(data_dir, prcp_file, sep  = '')
  prcp_var = readRDS(prcp_filepath) %>%
    dplyr::filter(id %in% viney_ids)

  # get the dates before
  for(stn_id in viney_ids){

    # do we to check any of these
    suspect_max_var <- max_var %>%
      dplyr::filter(wday(date) == 2 & id == stn_id)
    if(nrow(suspect_max_var) == 0){
      saveRDS(max_var, new_max_filepath)
      next
    }

    stn_prcp <- prcp_var %>%
      dplyr::filter(id == stn_id) %>%
      dplyr::filter(date %in% (suspect_max_var$date - 1)) %>%
      dplyr::filter(prcp == 0)

    if(nrow(stn_prcp) == 0){
      saveRDS(max_var, new_max_filepath)
      next
    }

    bad_dates = stn_prcp$date + 1

    max_var <- max_var %>%
      dplyr::mutate(qflag_prcp = if_else(date %in% bad_dates & id == stn_id,
                                         "ACCUM_SUNMON", qflag_prcp))
  }

  # save updated maximums
  saveRDS(max_var, new_max_filepath)

}

}
