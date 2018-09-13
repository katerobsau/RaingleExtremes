#' Wrapper for test for Sunday Monday Untagged Accumulations
#'
#' Checks for the presence of untagged Sunday Monday accumulations as in
#' Viney and Bates (2004).
#'
#' Test currently runs on yearly observations only. The block index returned
#' therefore corresponds to the year.
#'
#' @param stn_ids vector station ids to run the test
#' @param prcp_var standard rnoaa format for precipiation, PRCP,
#' see rnoaa::meteo_pull_monitors()
#' @param dapr_var standard rnoaa format for days accumulated precipitaiotn, DAPR,
#' see rnoaa::meteo_pull_monitors()
#'
#' @return Returns the a data frame with columns block, which is the year, and p_value.
#' @export
wrapper_viney <- function(stn_ids, prcp_var, dapr_var){

  output = mclapply(as.list(stn_ids),
                    sun_mon_untagged_test,
                    fun_prcp_var = prcp_var,
                    fun_dapr_var = dapr_var,
                    mc.cores = detectCores())

  num_rows = lapply(output, nrow) %>% unlist()
  id = rep(stn_ids, times = num_rows)
  output = do.call(rbind, output) %>% mutate(id = id)

  return(output)

}
