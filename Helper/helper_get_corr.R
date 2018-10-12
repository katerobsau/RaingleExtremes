data_dir = "/Users/saundersk1/Dropbox/Hard Drive/Data/GHCN-Daily/"
search_radius = 50
min_overlap = 3*365
temp_na_val <- 999.99
corr_type = "spearman"
file_name = paste(data_dir, "nbrs_radius", search_radius, ".rds", sep = "")
nbr_df = readRDS(file = file_name)

# -----------------------------------------------------------------------------

# for each prcp file get ids
# filter out the relevant pairs
# read in the pair data
# get the correlation

list_files = list.files(path = data_dir)
prcp_files = list_files[stringr::str_detect(list_files, pattern = "PRCP")]
num_files = length(prcp_files)
file_str = substr(prcp_files[1], 5, nchar(prcp_files[1]))
tail_str = paste("_dx_", strsplit(file_str, split = "_dx_")[[1]][[2]], sep ="")

print("WARNING: Hard code - assumed form of file_str")
corr_df <- data.frame(stn1 = NULL, stn2 = NULL, corr = NULL)
for(i in 1:num_files){

  # read in the precpitation data for that cell
  print(paste(i, "in", num_files))
  prcp_file = paste(data_dir, prcp_files[i], sep = "")
  prcp_var = readRDS(prcp_file)

  # get the station ids in the cell
  stn_ids = prcp_var %>%
    dplyr::select(id) %>%
    unlist() %>%
    as.vector() %>%
    unique()

  # get all possible pairs for stations in the cell (may also include nearby cells)
  pair_df <- nbr_df %>%
    dplyr::filter(stn1 %in% stn_ids | stn2 %in% stn_ids)
  if(nrow(pair_df) == 0) next

  # exclude any for which we've already estiamted the correlation
  if(nrow(corr_df) > 0){
    pair_df <- pair_df %>%
      dplyr::left_join(corr_df, by = c("stn1", "stn2")) %>%
      dplyr::filter(is.na(corr) == TRUE)
    if(nrow(pair_df) == 0) next
  }

  # read in any additional prcp data
  pair_ids <- pair_df %>%
    unlist() %>%
    as.vector() %>%
    unique()

  pair_meta <- as_meta_data %>%
    dplyr::filter(element == "PRCP") %>%
    dplyr::filter(id %in% pair_ids) %>%
    dplyr::mutate(xll = floor(longitude), yll = floor(latitude)) %>%
    dplyr::select(xll, yll) %>%
    dplyr::distinct() %>%
    dplyr::mutate(file_path =
                    paste(data_dir, "PRCP_xll_", xll, "_yll_", yll, tail_str, sep = ""))

  if(nrow(pair_meta) > 1){
    prcp_read = lapply(pair_meta$file_path,
                     function(l, prcp_file, prcp_var){
                      if(l == prcp_file) return(prcp_var)
                      prcp_data = readRDS(l)
                      return(prcp_data)
              }, prcp_file = prcp_file, prcp_var = prcp_var)
    prcp_var = do.call(rbind, prcp_read)
  }

  prcp_var <- prcp_var %>%
      dplyr::filter(id %in% pair_ids)

  # check we have the right data for pairs
  check = pair_ids %in% unique(prcp_var$id)
  if(!all(check)){
    false_ind = which(check == FALSE)
    rm_stns = pair_ids[false_ind]
    pair_df <- pair_df %>%
      dplyr::filter(!(stn1 %in% rm_stns) & !(stn2 %in% rm_stns))
    if(nrow(pair_df) == 0) next
  }

  # spread precipitation and get correlation
  print("Only consider quality observations")
  prcp_spread = prcp_var %>%
    dplyr::filter(qflag_prcp == " ") %>%
    dplyr::select(date, id, prcp) %>%
    tidyr::spread(key = id, value = prcp)

  # corr_results = apply(pair_df, 1, utils_correlation,
  #                    type = corr_type,
  #                    prcp_df = prcp_spread %>% select(-date),
  #                    min_overlap = min_overlap)

  corr_results = mcmapply(utils_correlation,
                          stn_pair = pair_df %>% t() %>% as.data.frame(),
                          MoreArgs = list(type = corr_type,
                                          prcp_df = prcp_spread %>% select(-date),
                                          min_overlap = min_overlap),
                          mc.cores = detectCores())

  # make sure to change the na value, so we can differentiate pairs
  # we've already calculated the correaltion for
  corr_results[is.na(corr_results)] = temp_na_val

  # update the correaltion data frame
  pair_df <- dplyr::mutate(pair_df, corr = corr_results)
  corr_df <- rbind(corr_df, pair_df)

}

corr_df <- corr_df %>%
  dplyr::filter(corr != temp_na_val) %>%
  dplyr::filter(!is.na(corr))

file_name = paste(data_dir, "corr_information1.rds", sep = "")
saveRDS(corr_df, file = file_name)
