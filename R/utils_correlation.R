#' Get the correlation between two stations
#'
#' @param prcp_df has columns of stn ids, but no date just precipitation data
#' @param stn_pair is a pair of stn ids - caution input must be characters!!!!
#' @param type correlation type, default is "pearson" otherwise, "spearman"
#' @param min_overlap is the number of observations needed before calculating corr(x,y)
#'
#' @return Returns the correlation between the observation at the two stations
#'
#' @examples
#' id1 = rexp(365*10, rate = 1)
#' id2 = id1 + rnorm(365*10, 0, 1)
#' prcp_df = data.frame(id1, id2)
#' stn_pair = c("id1", "id2")
#' utils_correlation(prcp_df, stn_pair)
#' utils_correlation(prcp_df, stn_pair, type = "spearman")
#' utils_correlation(prcp_df, stn_pair, type = "spearman", min_overlap = 100)
utils_correlation <- function(prcp_df, stn_pair, type = "pearson",
                             min_overlap = 365*3){

  stn_pair = as.character(stn_pair)
  prcp1 = prcp_df %>% dplyr::select(stn_pair[1]) %>% unlist %>% as.numeric()
  prcp2 = prcp_df %>% dplyr::select(stn_pair[2]) %>% unlist %>% as.numeric()

  common_obs = sum(!is.na(prcp1 + prcp2))
  if(common_obs < min_overlap){
    return(NA)
  }
  nonzero_obs = sum(prcp1 > 0 & prcp2 > 0, na.rm = T)
  if(common_obs < 100){
    return(NA)
  }

  corr_val = switch(type,
                    pearson = cor.test(prcp1, prcp2, method = "pearson")$estimate,
                    spearman = cor.test(prcp1, prcp2, method = "spearman")$estimate)

  return(corr_val)
}
