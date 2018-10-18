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
max_files = list_files[stringr::str_detect(list_files, pattern = "AM_xll")]
num_files = length(max_files)

for(i in 1:num_files){

  # read in the max4
  max_file = max_files[i]
  max4_file = paste("RAW_MAX4/", max_file, sep = "")
  max4_filepath = paste(data_dir, max4_file, sep  = '')
  max4_var = readRDS(max4_filepath)
  if(is.null(max4_var)) next
  max4_var = max4_var %>%
    dplyr::filter(max > 0)
  if(nrow(max4_var) == 0) next

  # get results
  stn_ids = unique(max4_var$id)
  len = length(stn_ids)
  king_results = data.frame(id = stn_ids, test = rep(NA, len), day = rep(NA, len))
  for(stn_ind in 1:len){
    stn_id = stn_ids[stn_ind]
    stn_prcp = max4_var %>%
      dplyr::filter(id == stn_id)
    date_vec = stn_prcp$date
    king_test = extremes_untagged_test(dates = date_vec,
                                alpha = king_alpha)
    if(all(is.na(king_test))) next
    if(any(king_test$bool == TRUE)){
      king_results$test[stn_ind] = TRUE;
      accum_day = (1:7)[which(king_test$bool == TRUE)]
      king_results$day[stn_ind] = accum_day
    }else{
      king_results$test[stn_ind] = FALSE;
    }
  }
  king_results <- king_results %>%
    dplyr::filter(test == TRUE)

  # save king results
  king_file = paste(c("KING", strsplit(max_files[i], split = "_")[[1]][-1]), collapse = "_")
  king_filepath = paste(data_dir, king_file, sep  = '')
  saveRDS(king_results, king_filepath)

}
