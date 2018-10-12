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
max_files = list_files[stringr::str_detect(list_files, pattern = "AM")]
num_files = length(max_files)

for(i in 1:num_files){

  # read king results
  king_file = paste(c("KING", strsplit(max_files[i], split = "_")[[1]][-1]), collapse = "_")
  kin_filepath = paste(data_dir, viney_file, sep  = '')
  king_results <- readRDS(king_filepath)

  # check if we need to worry
  if(nrow(king_results) == 0) next
  king_ids = king_results$id %>% unique()

  # now check relative to our data
  max_file = paste(max_data_dir, max_files[i], sep = "")
  max_var = readRDS(max_file)

  # read in the prcp data
  prcp_file = paste(c("PRCP", strsplit(max_files[i], split = "_")[[1]][-1]), collapse = "_")
  prcp_filepath = paste(data_dir, prcp_file, sep  = '')
  prcp_var = readRDS(prcp_filepath) %>%
    dplyr::filter(id %in% king_ids)

  # get the dates before
  for(stn_id in king_ids){

    bad_days = king_results %>%
      dplyr::filter(id == stn_id) %>%
      dplyr::select(day) %>%
      as.numeric()

    max_var <- max_var %>%
      dplyr::mutate(qflag_prcp = if_else(wday(date) %in% bad_days &
                                           id == stn_id &
                                           qflag_prcp != "ACCUM_SUNMON",
                                         "ACCUM_EXTREMES", qflag_prcp)) %>%
      dplyr::mutate(qflag_prcp = if_else(wday(date) %in% bad_days &
                                           id == stn_id &
                                           qflag_prcp == "ACCUM_SUNMON",
                                           "ACCUM_BOTH", qflag_prcp))
    # warning overright of possible outlier flags

  }

  saveRDS(max_var, max_file)

}
