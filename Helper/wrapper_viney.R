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
