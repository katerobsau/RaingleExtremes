recon_grid <- function(meta_data, cell_xll, cell_yll,
                       dx = 1, dy = 1, data_dir,
                       search_radius = 20, min_overlap = 100, delta_window = 30,
                       min_corr_value = 0.6, corr_type = "spearman",
                       min_years = 20,
                       end_year = 2017, start_year = 1910, qflags = c(" ")){

  # recon file
  recon_file =paste(data_dir, "/RECON/",
                    "RECON_xll_", cell_xll,
                    "_yll_", cell_yll,
                    "_dx_", dx, "_dy_", dy, ".rds", sep ="")
  if(file.exists(recon_file)){
    return(NULL)
  }

  # get all precipitation in nearby grid cells
  xll_vals = c(cell_xll - dx, cell_xll, cell_xll + dx)
  yll_vals = c(cell_yll - dy, cell_yll, cell_yll + dy)
  distinct_files = expand.grid(xll = xll_vals, yll = yll_vals) %>%
    as.data.frame()

  prcp_var = NULL
  for(i in 1:nrow(distinct_files)){
    xll = distinct_files$xll[i]
    yll = distinct_files$yll[i]
    prcp_file = paste(data_dir, "/","PRCP_xll_", xll, "_yll_", yll,
                      "_dx_", dx, "_dy_", dy, ".rds", sep ="")
    if(file.exists(prcp_file)){
      temp_prcp_var = readRDS(prcp_file)
      prcp_var = rbind(prcp_var, temp_prcp_var)
    }else{
      # print(paste(prcp_file, "does not exist"))
    }
  }

  # check the prcp data was found/provided
  if(is.null(prcp_var)){
    break("Error: no valid precipitation data provided")
  }

  # restrict this precipitaiton data to the appropriate quality flags
  prcp_var = prcp_var %>%
    mutate(prcp = if_else(qflag_prcp %in% qflags, prcp, NA_real_))

  # station ids within the cell we want to reconstruct
  cell_meta_data = meta_data %>%
    filter(longitude >= cell_xll & longitude < cell_xll + dx) %>%
    filter(latitude >= cell_yll & latitude < cell_yll + dy) %>%
    mutate(num_years = pmin(last_year, end_year) - pmax(first_year, start_year) + 1) %>%
    filter(num_years >= min_years)
  stn_ids = intersect(cell_meta_data$id, prcp_var$id)

  # reconstruct each of the stations
  recon_var = NULL
  for(stn_id in stn_ids){
    recon_prcp <- get_stn_recon(meta_data = meta_data,
                                stn_id = stn_id,
                                search_radius = search_radius,
                                from_file = FALSE, data_dir = NULL,
                                delta_window = delta_window, min_overlap = min_overlap,
                                min_corr_value = min_corr_value, corr_type = corr_type,
                                prcp_var = prcp_var, dx = dx, dy = dy)

    recon_var <- rbind(recon_var, recon_prcp)
  }

  # save the all the reconstructed data for a given cell to file
  saveRDS(recon_var, file = recon_file)

  return(NULL)

}

