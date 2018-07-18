#' Produces a spatial plot for a given set of dates and observations
#'
#' @param date_range dates for plotting
#' @param prcp_var standard rnoaa format for precipiation, PRCP
#' @param mdpr_var standard rnoaa format for accumulated precipitation totals, MDPR
#' @param dapr_var standard rnoaa format for days accumulated precipitation, DAPR
#' @param stn_meta_data the associated meta data for the stations,
#' must have columns of longitude and latitude
#'
#' @return Returns the plot
#'
#' @examples
#'
# date_range = seq(as.Date("1974-01-22"), as.Date("1974-01-30"),  by = "days")

plot_dates_spatially <- function(date_range, prcp_var, mdpr_var, dapr_var,
                                 stn_meta_data){
  #, stn_id, search_radius = 20){
  
  print("HARD CODE")
  text.type.large <- element_text(size = 12)
  text.type.small <- element_text(size = 10)
  
  
  # Visualise the data spatially for a set of dates
  subset_prcp_var <- dplyr::filter(prcp_var, date %in% date_range)
  subset_mdpr_var <- dplyr::filter(mdpr_var, date %in% date_range)
  subset_dapr_var <- dplyr::filter(dapr_var, date %in% date_range)

  # Combine with meta data so we can plot in space
  subset_prcp_var <- dplyr::left_join(subset_prcp_var, stn_meta_data, by = "id")

  # Combine the data for untagged accumulations
  select_prcp_var <- dplyr::select(subset_prcp_var, -mflag_prcp, -sflag_prcp)
  select_mdpr_var <- dplyr::select(subset_mdpr_var, id, date, mdpr)
  select_dapr_var <- dplyr::select(subset_dapr_var, id, date, dapr)
  tagged_var <- dplyr::left_join(select_mdpr_var, select_dapr_var, by = c("id", "date"))
  all_prcp_var <- dplyr::left_join(select_prcp_var, tagged_var, by = c("id", "date"))

  # Change the prcp types to numeric
  all_prcp_var$prcp = as.numeric(all_prcp_var$prcp)
  all_prcp_var$mdpr = as.numeric(all_prcp_var$mdpr)
  all_prcp_var$dapr = as.numeric(all_prcp_var$dapr)

  # Get data ready for plotting and combine prcp and mdpr
  plot_prcp_var <- all_prcp_var %>%
    dplyr::mutate(plot_prcp = pmax(prcp, mdpr, na.rm = TRUE)) #%>%

  # Create shape factor
  shape_code = rep(1, nrow(plot_prcp_var))
  shape_code[which(is.na(plot_prcp_var$plot_prcp))] = -1
  shape_code[which(plot_prcp_var$plot_prcp==0)] = 0
  accum_days = which(plot_prcp_var$dapr > 1)
  for(i in accum_days){
    stn_id = plot_prcp_var$id[i]
    stn_ind = which(plot_prcp_var$id == stn_id)
    accum_len = plot_prcp_var$dapr[i]
    accum_ind = (i - accum_len + 1):i
    ind = intersect(stn_ind, accum_ind)
    shape_code[ind] = 2
  }
  shape_code = as.factor(shape_code)
  plot_prcp_var = plot_prcp_var %>% dplyr::mutate(shape_code = shape_code)

  # Create the spatial plot
  label_code = c("NA", "ZERO", "NONZERO", "ACCUM")
  shape_value = c(4, 1, 19, 13)
  size_value = c(2, 4, 4, 4)
  if(!any(shape_code == -1)){
    rm = which(label_code == "NA")
    label_code = label_code[-rm]
    shape_value = shape_value[-rm]
    size_value = size_value[-rm]
  }
  if(!any(shape_code == 0)){
    rm = which(label_code == "ZERO")
    label_code = label_code[-rm]
    shape_value = shape_value[-rm]
    size_value = size_value[-rm]
  }
  if(!any(shape_code == 2)){
    rm = which(label_code == "ACCUM")
    label_code = label_code[-rm]
    shape_value = shape_value[-rm]
    size_value = size_value[-rm]
  }

  # stn_meta = stn_meta_data %>% filter(id == stn_id)
  spatial_plot <- ggplot(data = plot_prcp_var,
                       aes(x = longitude, y = latitude,
                           col = plot_prcp/10, shape = shape_code,
                           size = shape_code)) +
    geom_point() +
    scale_color_distiller(name = "PRCP (mm)", palette = "RdBu", direction = 1) +
    scale_shape_manual(name = "OBS", labels = label_code,
                     values = shape_value) +
    scale_size_manual(name = "OBS", labels = label_code,
                    values = size_value) +
    # scale_x_continuous(limits = c(stn_meta$longitude - search_radius/100,
    #                               stn_meta$longitude + search_radius/100)) +
    # scale_y_continuous(limits = c(stn_meta$latitude - search_radius/100,
                                  # stn_meta$latitude + search_radius/100)) +
    facet_wrap(~ date) +
    theme_bw() +
    xlab("Longitude") +
    ylab("Latitude") +
    coord_fixed() +
    theme(legend.text = text.type.small,
          axis.text = text.type.small,
          plot.title = text.type.large,
          legend.title = text.type.small,
          axis.title = text.type.small)

  # spatial_plot

  # library(plotly)
  # ggplotly(spatial_plot)
  return(spatial_plot)

}

