#' Find first observations that is not NA in the row
#'
#' @param df_row a row of precipitation observations,
#' first column is date, second columns is the station to be reconstructed,
#'  and the other rownames are neighbouring station ids.
#'  The values in all columns except the first are prcp.
#' @return Returns a data frame with columns of
#' date, id, recon_prcp and recon_flag
#'
#' @export
#'
#' @examples
#' date = seq(as.Date("1910-01-01"), as.Date("1910-01-12"), by = "days")
#' id1 = rexp(length(date), rate = 1)
#' id2 = id1 + rnorm(length(date), 0, 1)
#' id3 = id1 + rnorm(length(date), 0.5, 2)
#' prcp_df = data.frame(date, id1, id2, id3)
#' prcp_df$id1[1:10] = NA
#' prcp_df$id2[1:5] = NA
#' prcp_df$id3[1] = NA
#' recon_row(prcp_df[1,])
#' recon_row(prcp_df[2,])
#' recon_row(prcp_df[6,])
#' recon_row(prcp_df[11,])
#' recon_df = NULL
#' for(i in 1:length(date)){
#'   recon_df = rbind(recon_df, recon_row(prcp_df[i, ]))
#' }
#' recon_df
#' ### APPLY / SAPPLY ARE INHERITING NAMES WEIRDLY
#' ### apply(prcp_df[1:3, ], 1, recon_row)
#' ### This creates an error
recon_row <- function(df_row){

  date_val = df_row %>% select(date)
  df_row = df_row %>% select(-date)
  row_names = names(df_row)

  # which column indexes have observations
  non_NA_ind <- which(!is.na(df_row))

  if(length(non_NA_ind) == 0 | !is.na(df_row[1])){

    # if no suitable columns return NA
    recon_obs = data.frame(date = date_val,
                           id = row_names[1],
                           recon_prcp = NA,
                           recon_flag = NA,
                           stringsAsFactors = FALSE)
  }else{
    # if suitable columns can be used to infill,
    #flag the observation using its station id
    first_non_NA <- min(non_NA_ind)
    recon_prcp <- as.numeric(df_row[first_non_NA])
    recon_flag <- row_names[first_non_NA]
    recon_obs = data.frame(date = date_val,
                           id = row_names[1],
                           recon_prcp,
                           recon_flag,
                           stringsAsFactors = FALSE)
  }
  return(recon_obs)
}
