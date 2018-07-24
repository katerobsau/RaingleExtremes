#' Checks that the frequency of days rainfall extremes are observed is equal
#' throughout the days of the week, (King 2013)
#' Test runs on yearly observations, so the corresponding block index is the year.
#'
#' @param dates vector of dates
#' @param alpha alpha level of the two-sided hypothesis test (default = 0.05)
#' @param min_obs minimum number of observations needed to run the test (default = 40)
#'
#' @return Returns the a data frame with columns of wday for weekday, freq for frequency observation,
#' lower for whether the freq is below the low confidence interval,
#' upper for when the freq of the next day is above the confidence interval,
#' bool for when both the lower and upper indicate that there are untagged accumulations
#'
#' @export
#'
#' @examples
#'
#' dates = seq(as.Date("1910-01-01"), as.Date("1910-12-31"), by ="days")
#' extremes_untagged_test(dates)
#' monday_date = dates[wday(dates) == 2][1]
#' sunday_ind = which(wday(dates) == 1)
#' samp = sample(sunday_ind, 0.3*length(sunday_ind))
#' dates[samp] = monday_date
#' extremes_untagged_test(dates)
#'
extremes_untagged_test <- function(dates, alpha = 0.05, min_obs = 40){
  # input form is vector of dates yyyymmdd

  ci = c(alpha/2, 1 - alpha/2)
  n = length(dates)

  if(n < min_obs){
    print("Error: Reconsider using this test as there are too few observations")
    return(NA)
  }

  dates_wday = wday(dates)

  # bootstrapping
  # resamples from the origingal data with replacement R times
  R = 1000
  resamps = sapply(1:R, function(i){
    samp = sample(dates_wday, replace=T) %>%
      as.factor()
    levels(samp) = 1:7
    samp <- samp %>%
      table() %>%
      as.numeric()
    })

  # Binomial Distribution
  p = 1/7
  E = n*p
  sd = 2*sqrt(n/7*(1- 1/7))
  qLow = qbinom(ci[1], n, p);
  qUpp = qbinom(ci[2], n, p);
  # We have opted for a confidence intervals instead of a
  # standard deviation as used in King 2013

  est_freq = rowMeans(resamps)#/length(max_data$wday)
  est_freq = data.frame(wday = 1:7, freq = est_freq) %>%
    mutate(lower = freq < qLow) %>%
    mutate(upper = lead(freq > qUpp)) #%>%
  est_freq$upper[7] = est_freq$freq[1] > qUpp
  est_freq = est_freq %>% mutate(bool = lower & upper)
  est_freq$wday = as.factor(est_freq$wday)
  levels(est_freq$wday) = c("Sun" ,"Mon", "Tues", "Wed", "Thu", "Fri","Sat")

  #output
  return(est_freq)

}
