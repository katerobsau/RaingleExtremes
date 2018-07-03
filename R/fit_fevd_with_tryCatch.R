#' Get Stationary GEV parameters
#'
#' Fits a GEV distribution and returns the parameters.
#' This is a wrapper with a try catch statement to handle any errors
#'
#' @param x a vector of block maximum observations
#'
#' @return Returns stationary gev parameters.
#' If input vector x is invalid returns c(NA, NA, NA)
#'
#' @export
#'
#' @examples
#' x1 = revd(100,1,1,1)
#' par1 = fit_fevd_with_tryCatch(x1)
#' x2 = "SOMETHING SILLY"
#' par2 = fit_fevd_with_tryCatch(x2)
fit_fevd_with_tryCatch <- function(x){

  par = tryCatch({
    m = fevd(x = x, na.action = na.omit, type = "GEV")
    sum_m = summary(m, silent = TRUE)
    par = sum_m$par},
    error = function(e){return(rep(NA,3))}
  )

  return(par)

}

