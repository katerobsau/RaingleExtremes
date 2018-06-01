#' Test for Sunday Monday Untagged Accumulations
#'
#' Checks for the presence of untagged Sunday Monday accumulations as in
#' Viney and Bates (2004).
#'
#' Test currently runs on yearly observations only. The block index returned
#' therefore corresponds to the year.
#'
#' @param fun_prcp_var standard rnoaa format for precipiation, PRCP,
#' see rnoaa::meteo_pull_monitors()
#' @param fun_dapr_var standard rnoaa format for days accumulated precipitaiotn, DAPR,
#' see rnoaa::meteo_pull_monitors()
#' @param stn_id the station we want to consider
#' @param min_perc minimum proportion of weekday observations per year needed to run the test,
#' defaults to a half
#'
#' @return Returns the a data frame with columns block, which is the year, and p_value.
#' @export
#' @examples
#'
#' stn_id = "ASN00010525" #"ASN00022000"
#'
#' prcp_var <- meteo_pull_monitors(stn_id,
#'                                 date_min = "1910-01-01",
#'                                 date_max = "2000-01-01",
#'                                 keep_flags = TRUE,
#'                                 var = "PRCP")
#'
#' dapr_var <- meteo_pull_monitors(stn_id,
#'                                 date_min = "1910-01-01",
#'                                 date_max = "2000-01-01",
#'                                 keep_flags = TRUE,
#'                                 var = "DAPR")
#'
#' output <- sun_mon_untagged_test(prcp_var, dapr_var, stn_id)
#'
#' ggplot(output, aes(x = block, y = p_value)) +
#'   geom_hline(yintercept = c(0.0008, 0.5), linetype = "dashed", col = "darkgray", size = 1.1) +
#'   geom_point() +
#'   geom_line() +
#'   scale_y_log10() +
#'   xlab("Year") +
#'   ylab("P-Value") +
#'   ggtitle(paste("Test for Sunday Monday accumulations at", stn_id)) +
#'   theme_bw()
#'
sun_mon_untagged_test <- function(fun_prcp_var, fun_dapr_var, stn_id,
                                  min_perc = 0.5){

  # add a restriction on missing data per year
  min_wday_obs = (365/7*4)*min_perc
  min_sun_obs = 52*min_perc

  # restrict to the stn_id and the variables we need
  fun_prcp_var <- fun_prcp_var %>%
    filter(id == stn_id) %>%
    filter(qflag_prcp == " ") %>%
    select(-id, -mflag_prcp, -sflag_prcp, -qflag_prcp)

  # estimate p1
  p1_prob <- utils_p1(fun_prcp_var)
  p1_prob <- p1_prob %>%
    filter(num_obs >= min_wday_obs) %>%
    filter(p1 > 0)
  if(nrow(p1_prob) == 0){
    output = utils_empty_return(fun_prcp_var)
    return(output)
  }

  # count sundays
  count_sun = utils_count_sun(fun_prcp_var)
  if(nrow(count_sun) == 0){
    output = utils_empty_return(fun_prcp_var)
    return(output)
  }

  # probability of a rain on a sunday, given an accumulation
  fun_dapr_var <- filter(fun_dapr_var, id == stn_id)
  if(nrow(fun_dapr_var) > 0){

    # get dry wet probability
    p01_prob <- utils_p01(fun_prcp_var, p1_prob)
    if(nrow(p01_prob) == 0){
      output = utils_empty_return(fun_prcp_var)
      return(output)
    }

    # restrict to the stn_id and the variables we need
    fun_dapr_var <- fun_dapr_var %>%
      select(-id, -mflag_dapr, -sflag_dapr, -qflag_dapr)

    # combine prcp and dapr
    dapr_prcp_var <- fun_prcp_var %>%
      left_join(fun_dapr_var, by = c("date")) %>%
      filter(dapr > 1)

    # get probability of sunday rain during an accumulation
    ps_prob <- utils_ps(dapr_prcp_var, p1_prob, p01_prob)
    if(nrow(ps_prob) == 0){
      output = utils_empty_return(fun_prcp_var)
      return(output)
    }

    # count number of the tagged sundays per year
    tagged_count_sun <- utils_tagged_sun(ps_prob, count_sun)
    if(nrow(tagged_count_sun) != 0){
      count_sun = tagged_count_sun
    }
  }

  # preprocess for the binomial test
  count_sun = count_sun %>%
    filter(num_sun > min_sun_obs) %>%
    inner_join(p1_prob %>% select(block, p1), by = "block")
  if(nrow(count_sun) == 0){
    output = utils_empty_return(fun_prcp_var)
    return(output)
  }

  # get the p_values for the test
  p_value = mapply(utils_binom,
                   x = count_sun$num_rainy_sun,
                   n = count_sun$num_sun,
                   p = count_sun$p1)

  output = data.frame(block = count_sun$block, p_value = p_value)
  return(output)

}

