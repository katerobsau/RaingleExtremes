### Save my data
data_dir = "/Users/saundersk1/Dropbox/Hard Drive/Data/GHCN-Daily/"
load("Data/as_meta_data.RData")

#------------------------------------------------------------------------------

list_files = list.files(path = data_dir)
prcp_files = list_files[stringr::str_detect(list_files, pattern = "PRCP")]
num_files = length(prcp_files)

for(i in 1:num_files){

  file_str = substr(prcp_files[i], 5, nchar(prcp_file))

  prcp_file = paste(data_dir, prcp_files[i], sep = "")
  prcp_var = readRDS(prcp_file)

  dapr_file = paste(data_dir, "DAPR", file_str, sep = "")
  dapr_var = readRDS(dapr_file)

  stn_ids = prcp_var %>%
    dplyr::select(id) %>%
    unlist() %>%
    as.vector() %>%
    unique()

  viney_result = wrapper_viney(stn_ids = stn_ids,
                               prcp_var = prcp_var,
                               dapr_var = dapr_var)

  viney_file = paste(data_dir, "VINEY", file_str, sep ="")

  saveRDS(viney_result, file = viney_file)

}
