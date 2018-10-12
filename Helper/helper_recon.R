# reconstruct the stations

# get all stations that have neighbours
# filter by level of correlation
# sort the neighbours by the correlation, decreasing = TRUE
# combine the prcp for each of these
# get the first non-zero entry in each row
# save out as recon with tail_str
library(dplyr)
library(parallel)
library(tidyr)
library(foreach)

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
  source(paste(wd, "recon_stn.R", sep = "/"))
  source(paste(wd, "recon_row.R", sep = "/"))
  source(paste(wd, "check_obs_in_window.R", sep = "/"))
}

min_dist = 30
min_corr = 0.6

corr_file_name = paste(data_dir, "corr_information.rds", sep = "")
corr_df <- readRDS(file = corr_file_name)

dist_file_name = paste(data_dir, "close_nbrs_dist.rds", sep = "")
dist_df <- readRDS(file = dist_file_name)


# restrict the recontrution to pairs of min_dist to each other
# and of min_corr to each other

corr_df <- corr_df %>%
  dplyr::left_join(dist_df, by= c("stn1", "stn2")) %>%
  dplyr::filter(km <= min_dist)  %>%
  dplyr::filter(corr > min_corr)

# sort the stations into the order in which we want to reconstruct
temp_corr_function <- function(id, corr_df){

  corr_stn <- corr_df %>%
    dplyr::filter(stn1 == id | stn2 == id) %>%
    dplyr::arrange(desc(corr)) %>%
    dplyr::mutate(stn = if_else(stn1 == id, stn2, stn1))

  return(corr_stn$stn)

}

corr_ids = corr_df %>%
  select(stn1, stn2) %>%
  unlist() %>%
  unique()

recon_order <- mclapply(corr_ids,
                        FUN = temp_corr_function,
                        corr_df = corr_df,
                        mc.cores = detectCores())

# profvis({
list_files = list.files(path = data_dir)
prcp_files = list_files[stringr::str_detect(list_files, pattern = "PRCP")]

num_files = length(prcp_files)
print("HARD CODED FILE STR")
cell_str = paste("_dx_", strsplit(prcp_files[1], split = "_dx_")[[1]][[2]], sep ="")
for(i in 1:num_files){

  # file_str = substr(prcp_files[i], 5, nchar(prcp_files[1]))
  # file_name = paste(data_dir, "RECON", file_str, sep = "")
  # if(file.exists(file_name)) next

  # read in the precpitation data for that cell
  print(paste(i, "in", num_files))
  prcp_file = paste(data_dir, prcp_files[i], sep = "")
  print(prcp_file)

  # hacked these lines in to deal with the failed read on the server
  file_str = substr(prcp_files[i], 5, nchar(prcp_files[1]))
  file_str_parts = strsplit(file_str, split ="_")[[1]]
  cell_xll = file_str_parts[3] %>% as.numeric()
  cell_yll = file_str_parts[5] %>% as.numeric()
  # if(cell_xll <= 147) next # file already exists
  # print("Warning: hacked the above code chunk in")

  # read in the precpitation data for that cell
  prcp_var = readRDS(prcp_file)
  cell_ids = prcp_var %>%
    select(id) %>%
    unlist() %>%
    unique()

  # check if there are neighbours that are suitable for reconstruction
  recon_info <- corr_df %>%
    dplyr::filter(stn1 %in% cell_ids | stn2 %in% cell_ids)

  # in no  - don't need to reconstruct, so create a dummy output file
  if(nrow(recon_info) == 0){
    file_str = substr(prcp_files[i], 5, nchar(prcp_files[1]))
    file_name = paste(data_dir, "RECON", file_str, sep = "")
    recon_var = prcp_var %>%
      dplyr::mutate(recon_prcp = prcp, recon_flag = NA)
    # THIS FORMULATION DIFFERS FROM OUR OTHER ASSUMPTIONS
    # SEE RECON_STN()
    saveRDS(recon_var, file = file_name)
    next
  }

  # if yes - get nbrs
  nbr_ids <- recon_info %>%
    select(stn1, stn2) %>%
    unlist() %>%
    unique()

  # read in any additional data needed
  recon_meta <- as_meta_data %>%
    dplyr::filter(element == "PRCP") %>%
    dplyr::filter(id %in% nbr_ids) %>%
    dplyr::mutate(xll = floor(longitude), yll = floor(latitude)) %>%
    dplyr::select(xll, yll) %>%
    dplyr::distinct() %>%
    dplyr::mutate(file_path =
                    paste(data_dir, "PRCP_xll_", xll, "_yll_", yll, cell_str, sep = ""))

  # cleanly handles file if we need to read in more files
  prcp_read = lapply(recon_meta$file_path,
                     function(l, prcp_file, prcp_var){
                       if(l == prcp_file) return(prcp_var)
                       prcp_data = readRDS(l)
                       return(prcp_data)
                     }, prcp_file = prcp_file, prcp_var = prcp_var)
  all_prcp_var = do.call(rbind, prcp_read)

  # want to reconstruct each station
  # so get the ones in the cell
  # loop over them
  all_ids_for_recon <- intersect(nbr_ids, cell_ids)
  num_recon = length(all_ids_for_recon)

  # all_recon_df <- NULL
  # for(j in 1:num_recon){
  #
  #   print(paste(Sys.time(), j, "in", num_recon))
  #   stn_id_for_recon <- all_ids_for_recon[j]
  #   recon_ord_ind = which(stn_id_for_recon == corr_ids)
  #   recon_ids <- c(stn_id_for_recon, recon_order[[recon_ord_ind]])
  #
  #   # only need to reconstruct missing data
  #   prcp_spread = all_prcp_var %>%
  #     dplyr::filter(id %in% recon_ids) %>%
  #     dplyr::filter(qflag_prcp == " ") %>%
  #     dplyr::select(date, id, prcp) %>%
  #     tidyr::spread(key = id, value = prcp)
  #
  #   # reconstruct the station
  #   recon_data <- recon_stn(prcp_spread)
  #   all_recon_df <- rbind(all_recon_df, recon_data)
  #
  # }

temp_loop_function <- function(j, all_ids_for_recon, corr_ids, recon_order, all_prcp_var){

    print(paste(Sys.time(), j, "in", num_recon))
    stn_id_for_recon <- all_ids_for_recon[j]
    recon_ord_ind = which(stn_id_for_recon == corr_ids)
    recon_ids <- c(stn_id_for_recon, recon_order[[recon_ord_ind]])

    # only need to reconstruct missing data
    prcp_spread = all_prcp_var %>%
      dplyr::filter(id %in% recon_ids) %>%
      dplyr::filter(qflag_prcp == " ") %>%
      dplyr::select(date, id, prcp) %>%
      tidyr::spread(key = id, value = prcp)

    # reconstruct the station
    recon_data <- recon_stn(prcp_spread)

    return(recon_data)

}

if(wd == local_dir){
cl <- parallel::makeCluster(detectCores())
all_recon_df <- foreach(j = 1:num_recon, .combine = bind_rows,
        .packages = c('RaingleExtremes', 'dplyr', 'tidyr')) %dopar%
  temp_loop_function(j, all_ids_for_recon, corr_ids,
                     recon_order, all_prcp_var)
stopCluster(cl)
}else{
  print('ON THE SERVER')
  cl <- parallel::makeCluster(detectCores())
  all_recon_df <- foreach(j = 1:num_recon, .combine = bind_rows,
                          .packages = c('dplyr', 'tidyr')) %dopar%
    temp_loop_function(j, all_ids_for_recon, corr_ids, recon_order, all_prcp_var)
  stopCluster(cl)
}

  file_str = substr(prcp_files[i], 5, nchar(prcp_files[1]))
  file_name = paste(data_dir, "RECON", file_str, sep = "")
  recon_var = prcp_var %>%
    dplyr::left_join(all_recon_df, by = c("id", "date"))

  saveRDS(recon_var, file = file_name)

}

# })

###----------------------------------------------------------------------------
#
# list_files = list.files(path = data_dir)
# prcp_files = list_files[stringr::str_detect(list_files, pattern = "PRCP")]
# num_files = length(prcp_files)
# file_str = substr(prcp_files[1], 5, nchar(prcp_files[1]))
# tail_str = paste("_dx_", strsplit(file_str, split = "_dx_")[[1]][[2]], sep ="")
#
# recon_ids <- c(corr_ids[4], recon_order[[4]])
#
# recon_meta <- as_meta_data %>%
#   dplyr::filter(element == "PRCP") %>%
#   dplyr::filter(id %in% recon_ids) %>%
#   dplyr::mutate(xll = floor(longitude), yll = floor(latitude)) %>%
#   dplyr::select(xll, yll) %>%
#   dplyr::distinct() %>%
#   dplyr::mutate(file_path =
#                   paste(data_dir, "PRCP_xll_", xll, "_yll_", yll, tail_str, sep = ""))
#
# prcp_read = lapply(recon_meta$file_path,
#                    function(l, prcp_file){
#                      prcp_data = readRDS(l)
#                      return(prcp_data)
#                    }, prcp_file = prcp_file)
# prcp_var = do.call(rbind, prcp_read)
#
# # spread precipitation and get correlation
# prcp_spread = prcp_var %>%
#   dplyr::filter(id %in% recon_ids) %>%
#   dplyr::filter(qflag_prcp == " ") %>%
#   dplyr::select(date, id, prcp) %>%
#   tidyr::spread(key = id, value = prcp) %>%
#   dplyr::select(date, recon_ids)
#
# recon_data <- recon_stn(prcp_spread)

###----------------------------------------------------------------------------

