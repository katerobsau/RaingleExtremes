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

max_suspect = 10
max_recon_allowed = 10
min_block = 20
max_data_dir = paste(data_dir, "RCON_MAX/", sep = "")
list_files = list.files(path = max_data_dir)
max_files = list_files[stringr::str_detect(list_files, pattern = "AM")]
num_files = length(max_files)
max_data = data.frame(block = 1910:2017)
for(i in 1:num_files){

  # read in our max data
  max_file = paste(max_data_dir, max_files[i], sep = "")
  print(max_file)
  max_var = readRDS(max_file)
  if(is.null(max_var)) next

  # add a hack in here because of the recon flag weirdness I need to fix
  # actually works with a later code chunk anyway
  recon_dates = max_var$date[which(!is.na(max_var$recon_flag))] %>% unique()
  max_var = max_var %>%
    dplyr::mutate(recon_flag = if_else(date %in% recon_dates, 'FLAG', recon_flag)) %>%
    dplyr::distinct()

  # standardarise the format
  block = max_var %>% select(block)
  std_max_var = max_var %>%
    dplyr::mutate(max = if_else(qflag_prcp %in% c(" ","O"), max, NA_real_)) %>%
    dplyr::select(block, id, max) %>%
    tidyr::spread(key = id, value = max) %>%
    dplyr::select(-block)

  # count the number with suspect flags
  suspect_ids <- max_var %>%
    dplyr::filter(max > 0) %>%
    dplyr::filter(qflag_prcp != " ") %>%
    dplyr::count(id) %>%
    dplyr::filter(n > max_suspect) %>%
    dplyr::select(id) %>%
    unlist() %>%
    as.vector()

  # count the number with reconstructed flags
  rm_recon_ids <- max_var %>%
    dplyr::filter(max > 0) %>%
    dplyr::filter(recon_flag == 'FLAG') %>%
    dplyr::count(id) %>%
    dplyr::filter(n > max_recon_allowed) %>%
    dplyr::select(id) %>%
    unlist() %>%
    as.vector()

  # count observations per station
  col_sums = colSums(!is.na(std_max_var))
  if(any(col_sums < min_block)){
    few_ids = names(std_max_var)[which(col_sums < min_block)]
  }else{
    few_ids = NULL
  }

  # remove any stations with too little data, too many reconstructed,
   # or too many missing
  rm_ids = c(few_ids, rm_recon_ids, suspect_ids)
  keep_ids = setdiff(names(std_max_var), rm_ids)
  std_max_var <- std_max_var %>%
      dplyr::select(keep_ids)
  std_max_var <- cbind(block, std_max_var)

  # combine in main data frame
  max_data = dplyr::left_join(max_data, std_max_var, by = "block")

}

# std_max_file = paste(wd, "/", block_name, "_summary.rds", sep = "")
# saveRDS(max_data, std_max_file)
