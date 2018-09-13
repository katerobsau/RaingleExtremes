#' Reconstructs missing observations at a station by finding the
#' first observation in each row that is not NA
#'
#' @param prcp_df a df of precipitation observations,
#' first column is date, second columns is the station to be reconstructed,
#'  and the other rownames are neighbouring station ids.
#'  The values the first column are dates and
#'  the values are precipitation in all columns
#' @param delta_window (default = 30) see documentation for check_obs_in_window
#' @return Returns a data frame with columns of
#' date, id, recon_prcp and recon_flag
#'
#' @export
#'
#' @examples
#' date = seq(as.Date("1910-01-01"), as.Date("1910-01-31"), by = "days")
#' id1 = rexp(length(date), rate = 1)
#' id2 = id1 + rnorm(length(date), 0, 1)
#' id3 = id1 + rnorm(length(date), 0.5, 2)
#' prcp_df = data.frame(date, id1, id2, id3)
#' View(recon_stn(prcp_df))
#' prcp_df$id1[1:10] = NA
#' prcp_df$id2[1:5] = NA
#' prcp_df$id3[1] = NA
#' View(recon_stn(prcp_df))
#' View(recon_stn(prcp_df, delta_window = 3))
recon_stn <- function(prcp_df, delta_window){

  # check the columns are what we need
  if(names(prcp_df)[1] != "date"){
    break("Incorrect columns specified")
  }

  # set up a default data frame to return
  stn_id = names(prcp_df)[2]
  recon_stn = data.frame(date = prcp_df$date,
                         id = stn_id,
                         recon_prcp = NA,
                         recon_flag = NA,
                         stringsAsFactors = FALSE)

  # if there are no neighbours for reconstruction
  if(ncol(prcp_df) == 2){
    return(recon_stn)
  }

  # which prcp needs recon - NA obs
  na_rows = which(is.na(prcp_df[,2]))
  if(length(na_rows) == 0){
    return(recon_stn)
  }

  # check if there is an observation at the station within a given window
  # don't want to reconstruct large gaps
  stn_prcp = prcp_df[,2] %>% unlist() %>% as.numeric()
  if(missing(delta_window)){
    obs_in_window = rep(TRUE, length(na_rows))
  }else{
    obs_in_window = check_obs_in_window(na_rows, prcp = stn_prcp,
                         delta_window)
  }

  # reconstruct mising observations
  # didn't want to write this in a for loop, but
  # had trouble with apply statement in recon_row
  # for(i in 1:length(na_rows)){
  #   row_ind = na_rows[i]
  #   if(obs_in_window[i] == TRUE){
  #     recon_stn_row = recon_row(prcp_df[row_ind,])
  #     recon_stn[row_ind, ] = recon_stn_row
  #   }
  # }

  na_rows = na_rows[obs_in_window == TRUE]
  if(length(na_rows) == 0) return(recon_stn)

  recon_list <- mclapply(as.list(na_rows),
                         function(row_ind, prcp_df){
                            recon_output = recon_row(prcp_df[row_ind, ])
                            return(recon_output)
      },
      prcp_df = prcp_df,
      mc.cores = detectCores())

  recon_stn = do.call(rbind, recon_list)

  return(recon_stn)
}
