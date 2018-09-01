#' Gets station data from GHCN-Daily for a given set of stations IDs
#'
#' Can also pass all inputs from rnoaa:meteo_pull_monitors()
#'
#' @param stn_ids a set of station ids
#' @param data_dir the directory to save the data
#' @param file_str a reference string for the file (default = "")
#'
#' @return Returns NULL, but saves the data for all station ids into
#' separate files for each of the element types. The outpute files take the form
#' paste(data_dir, element_type, file_str, ".rds", sep ="")
#'
#' @export
#'
wrapper_save_prcp_data <- function(stn_ids, data_dir, file_str = "", ...){

  # Get all the relevant precipitation elements
  prcp_var <- meteo_pull_monitors(monitors = stn_ids,
                                  keep_flags = TRUE,
                                  var = "PRCP", ...)

  mdpr_var <- meteo_pull_monitors(monitors = stn_ids,
                                  keep_flags = TRUE,
                                  var = "MDPR", ...)

  dapr_var <- meteo_pull_monitors(monitors = stn_ids,
                                  keep_flags = TRUE,
                                  var = "DAPR", ...)

  # dwpr_var <- meteo_pull_monitors(monitors = stn_ids,
  #                                 keep_flags = TRUE,
  #                                 var = "DWPR", ...)

  # Set up file names
  prcp_file = paste(data_dir, "PRCP", file_str, ".rds", sep ="")
  mdpr_file = paste(data_dir, "MDPR", file_str, ".rds", sep ="")
  dapr_file = paste(data_dir, "DAPR", file_str, ".rds", sep ="")
  # dwpr_file = paste(data_dir, "DWPR", file_str, ".rds", sep ="")

  #Save out out the prcp variables
  saveRDS(prcp_var, file = prcp_file)
  saveRDS(mdpr_var, file = mdpr_file)
  saveRDS(dapr_var, file = dapr_file)
  saveRDS(dapr_var, file = dwpr_file)

  return(NULL)

}
