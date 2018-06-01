#' Checks if there a observations in a given window
#'
#' @param i index of the observation
#' @param prcp a vector of daily precpitation observations (these are assumed sequential)
#' @param delta_window window to check, i is in the centre of this window (default = 30)
#' @return Returns a boolean if there a observations in the window
#'
#' @examples
#' prcp = 1:365
#' prcp[100:120] = NA
#' check_obs_in_window(i = 100, prcp)
#' check_obs_in_window(i = seq(10, 360, 50), prcp, delta_window = 10)
#' check_obs_in_window(i = seq(10, 360, 50), prcp)
check_obs_in_window <- function(i, prcp, delta_window = 30){
  first_ind = pmax(rep(1, length(i)),
                  i - floor(delta_window/2))
  end_ind = pmin(rep(length(prcp), length(i)),
                     i + ceiling(delta_window/2))
  # let prcp be defined globally  (bit of naughty coding)
  temp_fun <- function(first_ind, end_ind){
    bool_val = any(!is.na(prcp[first_ind:end_ind]))
    return(bool_val)
  }
  obs_in_window = mapply(temp_fun, first_ind, end_ind)
  return(obs_in_window)
}
