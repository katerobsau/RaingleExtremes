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
  source(paste(wd, "run_king_test.R", sep = "/"))
}

king_alpha = 0.05
max_data_dir =paste(data_dir, "RCON_MAX/", sep = "")
list_files = list.files(path = max_data_dir)
block_patterns = c("AM", "SPR", "WIN", "SUM", "AUT")
for(block_pattern in block_patterns){

  block_pattern = paste(block_pattern, "VINEY", sep = "_")
  max_files = list_files[stringr::str_detect(list_files, pattern = block_pattern)]
  num_files = length(max_files)

for(i in 1:num_files){

  print(paste(block_pattern, i, "in", num_files))

  # now check relative to our data
  max_file = paste(max_data_dir, max_files[i], sep = "")
  max_var = readRDS(max_file)

  # new max files
  max_parts = strsplit(max_files[i], split = "_")[[1]]
  max_start = paste(max_parts[1], "FLAGS", sep='_')
  new_max_file = paste(c(max_start, max_parts[-1]), collapse = "_")
  new_max_filepath = paste(max_data_dir, new_max_file, sep  = '')
  print(new_max_filepath)

  # read king results
  king_file = paste(c("KING", strsplit(max_files[i], split = "_")[[1]][-(1:2)]), collapse = "_")
  king_filepath = paste(data_dir, king_file, sep  = '')
  if(!file.exists(king_filepath)){
    saveRDS(max_var, new_max_filepath)
    next
  }
  king_results <- readRDS(king_filepath) %>% dplyr::filter(test == TRUE)

  # check if we need to worry
  if(nrow(king_results) == 0){
    saveRDS(max_var, new_max_filepath)
    next
  }
  king_ids = king_results$id %>% unique()

  # read in the prcp data
  prcp_file = paste(c("PRCP", strsplit(max_files[i], split = "_")[[1]][-(1:2)]), collapse = "_")
  prcp_filepath = paste(data_dir, prcp_file, sep  = '')
  prcp_var = readRDS(prcp_filepath) %>%
    dplyr::filter(id %in% king_ids)

  # get the dates before
  for(stn_id in king_ids){

    # get suspect days
    bad_days = king_results %>%
      dplyr::filter(id == stn_id) %>%
      dplyr::select(day) %>%
      as.numeric()

    # do we to check any of these
    suspect_max_var <- max_var %>%
      dplyr::filter(id == stn_id) %>%
      dplyr::filter(wday(date) %in% bad_days)
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

    bad_days = stn_prcp$date + 1

    max_var <- max_var %>%
      dplyr::mutate(qflag_prcp = if_else(date %in% bad_days &
                                           id == stn_id &
                                           qflag_prcp != "ACCUM_SUNMON",
                                         "ACCUM_EXTREMES", qflag_prcp)) %>%
      dplyr::mutate(qflag_prcp = if_else(date %in% bad_days &
                                           id == stn_id &
                                           qflag_prcp == "ACCUM_SUNMON",
                                           "ACCUM_BOTH", qflag_prcp))
    # warning overright of possible outlier flags

  }

  # save updated maximums
  saveRDS(max_var, new_max_filepath)

}
}
