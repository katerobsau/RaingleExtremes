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
max_files = list_files[stringr::str_detect(list_files, pattern = "AM")]
num_files = length(max_files)

for(i in 1:num_files){

  # run viney test (helper_viney.R)
  viney_file = paste(c("VINEY", strsplit(max_files[i], split = "_")[[1]][-1]), collapse = "_")
  viney_filepath = paste(data_dir, viney_file, sep  = '')
  viney_result = readRDS(viney_filepath)

  # check if we need to worry
  if(is.null(viney_result)) next

  # filter by suspect pvalues
  viney_result <- viney_result %>%
    dplyr::filter(p_value < crit_viney_pval)

  # check again if we need to do any data handling
  if(nrow(viney_result) == 0) next
  viney_ids = unique(viney_result$id)

  # read in our max data
  max_file = paste(max_data_dir, max_files[i], sep = "")
  print(max_file)
  max_var = readRDS(max_file)

  # do we to check any of these
  suspect_max_var <- max_var %>%
    dplyr::filter(wday(date) == 2 & id %in% viney_ids)
  if(nrow(suspect_max_var) == 0) next

  # read in the prcp data
  prcp_file = paste(c("PRCP", strsplit(max_files[i], split = "_")[[1]][-1]), collapse = "_")
  prcp_filepath = paste(data_dir, prcp_file, sep  = '')
  prcp_var = readRDS(prcp_filepath) %>%
    dplyr::filter(id %in% viney_ids)

  # get the dates before
  for(stn_id in viney_ids){

    stn_prcp <- prcp_var %>%
      dplyr::filter(id == stn_id) %>%
      dplyr::filter(date %in% lag(suspect_max_var$date)) %>%
      dplyr::filter(prcp == 0)

    if(nrow(stn_prcp) == 0) next

    bad_dates = stn_prcp$date + 1

    max_var <- max_var %>%
      dplyr::mutate(qflag_prcp = if_else(date %in% bad_dates & id == stn_id,
                                         "ACCUM_SUNMON", qflag_prcp))
  }

  saveRDS(max_var, max_file)

}
