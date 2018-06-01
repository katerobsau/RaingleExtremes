### viney bates util functions

# empty return for insufficent data
utils_empty_return <- function(fun_prcp_var){
  output = fun_prcp_var %>%
    mutate(block = year(date)) %>%
    select(block) %>%
    distinct() %>%
    mutate(p_value = NA)
  return(output)
}

# get estimate p1
utils_p1 <- function(fun_prcp_var){

  # preprocess to get probability of a rainy day
  p1_preprocess <- fun_prcp_var %>%
    filter(!is.na(prcp)) %>%
    filter(wday(date) %in% 3:6) %>% # TUES - FRI
    mutate(block = year(date)) %>% # Create a block index
    mutate(num_obs = (prcp >= 0)*block) %>%
    mutate(num_nonzero_obs = (prcp > 0)*block)

  # get number of observed days per block
  num_obs = p1_preprocess %>%
    count(num_obs)
  names(num_obs) = c("block", "num_obs")

  # get number of rainy days per block
  num_rainy = p1_preprocess %>%
    filter(num_nonzero_obs != 0) %>%
    count(num_nonzero_obs)
  names(num_rainy) = c("block", "num_rainy")

  # get the probability p1 of a rainy day per block
  p1_prob = inner_join(num_obs, num_rainy, by = "block") %>%
    mutate(num_rainy = ifelse(is.na(num_rainy), 0, num_rainy)) %>%
    mutate(p1 = num_rainy/num_obs)

  return(p1_prob)

}

# count sundays
utils_count_sun <- function(fun_prcp_var){

  # number of sundays per year
  num_sun <- fun_prcp_var %>%
    filter(wday(date) == 1 & prcp >= 0) %>%
    mutate(block = year(date)) %>%
    select(-date) %>%
    count(block)
  names(num_sun) = c("block", "num_sun")

  # number of rainy sundays per year
  num_rainy_sun <- fun_prcp_var %>%
    filter(wday(date) == 1 & prcp > 0) %>%
    mutate(block = year(date)) %>%
    select(-date) %>%
    count(block)
  names(num_rainy_sun) = c("block", "num_rainy_sun")

  count_sun <- left_join(num_sun, num_rainy_sun, by = "block") %>%
    mutate(num_rainy_sun = ifelse(is.na(num_rainy_sun), 0, num_rainy_sun))

  return(count_sun)

}

utils_p01 <- function(fun_prcp_var, p1_prob){
  # conditial probability of a rainy day after a dry day
  p01_preprocess = fun_prcp_var %>%
    mutate(prev_prcp = lag(prcp)) %>%
    filter(!is.na(prcp)) %>%
    filter(!is.na(prev_prcp)) %>%
    mutate(dry_wet = prcp > 0 & prev_prcp == 0) %>%
    mutate(dry_dry = prcp == 0 & prev_prcp == 0) %>%
    filter(wday(date) %in% 4:6) %>% # WED - FRI
    mutate(block = year(date)) %>% # Create a block index
    mutate(num_dry_wet = dry_wet*block) %>%
    mutate(num_dry_dry = dry_dry*block) %>%
    mutate(num_dry_dot = num_dry_wet + num_dry_dry)

  # count the number of dry then wet days
  num_dry_wet = p01_preprocess %>%
    count(num_dry_wet)
  names(num_dry_wet) = c("block", "num_dry_wet")

  # count the number of two day observations, where the first day is dry
  num_dry_dot = p01_preprocess %>%
    count(num_dry_dot)
  names(num_dry_dot) = c("block", "num_dry_dot")

  # get the probability of a dry then wet day
  p01_prob = left_join(num_dry_dot, num_dry_wet, by = "block") %>%
    inner_join(p1_prob %>% select(p1, block), by = "block") %>%
    mutate(p0_and_1 = num_dry_wet/num_dry_dot) %>%
    mutate(p0 = 1 - p1) %>%
    mutate(p01 = p0_and_1/p0)

  return(p01_prob)

}

# get probability of sunday rain in an accumulation
utils_estimate_ps <- function(p1, p01, a){

  p1 = as.numeric(p1)
  p01 = as.numeric(p01)
  a = as.numeric(a)
  if(any(is.na(c(p1, p01, a)))) return(NA)
  denom = 1 - (1 - p1)*((1 - p01))^(a-1)
  ps = p1/denom
  return(ps)

}

# get number of sundays per accumulation
utils_num_sun <- function(a, date_val){

  if(is.na(a)){return(NA)}
  a = as.numeric(a)
  date_seq = seq(as.Date(date_val) - a + 1,
                 as.Date(date_val), by = "days")
  num_sun = sum(wday(date_seq) == 1)
  return(num_sun)

}

utils_ps <- function(dapr_prcp_var, p1_prob, p01_prob){

  # preprocess to get rainfall on a sunday in an accumulation
  ps_preprocess <- dapr_prcp_var %>%
    mutate(block = year(date)) %>%
    inner_join(p1_prob %>% select(p1, block), by = "block") %>%
    inner_join(p01_prob %>% select(p01, block), by = "block")

  if(nrow(ps_preprocess) == 0){
    return(data.frame(empty = NULL))
  }

  # get the probability of rain on sunday in an accumulation
  ps_results = mapply(utils_estimate_ps, ps_preprocess$p1, ps_preprocess$p01,
                    ps_preprocess$dapr)

    # count sundays in the accumulation
  num_sun_results = mapply(utils_num_sun, ps_preprocess$dapr,
                         ps_preprocess$date)

  # get the expected number of sundays that had rainfall in the accumulation
  ps_prob <- ps_preprocess %>%
    mutate(ps = ps_results) %>%
    mutate(num_tagged_sun = num_sun_results) %>%
    mutate(exp_sun = round(num_tagged_sun*ps))

  return(ps_prob)
}

utils_tagged_sun <- function(ps_prob, count_sun){

  ps_count = ps_prob %>%
    filter(num_tagged_sun >= 1) %>%
    select(block, num_tagged_sun)

  if(nrow(ps_count) == 0){
    return(data.frame(empty = NULL))
  }

  num_tagged_sun = rep(ps_count$block, times = ps_count$num_tagged_sun) %>%
      as.data.frame()
  names(num_tagged_sun) = "block"
  num_tagged_sun <- num_tagged_sun %>% count(block)
  names(num_tagged_sun) = c("block", "num_tagged_sun")

  # count the number of the tagged rainy sundays per year
  ps_exp_count = ps_prob %>%
    filter(exp_sun > 0) %>%
    select(block, exp_sun)
  num_tagged_rainy_sun = rep(ps_exp_count$block, times = ps_exp_count$exp_sun) %>%
      as.data.frame()
  names(num_tagged_rainy_sun) = "block"
  num_tagged_rainy_sun <- num_tagged_rainy_sun %>% count(block)
  names(num_tagged_rainy_sun) = c("block", "num_tagged_rainy_sun")

  # join all the counts together
  count_sun <- count_sun %>%
    left_join(num_tagged_sun, by = "block") %>%
    mutate(num_tagged_sun =
           ifelse(is.na(num_tagged_sun), 0, num_tagged_sun)) %>%
    left_join(num_tagged_rainy_sun, by = "block") %>%
    mutate(num_tagged_rainy_sun =
           ifelse(is.na(num_tagged_rainy_sun), 0, num_tagged_rainy_sun)) %>%
    mutate(num_sun = num_sun + num_tagged_sun) %>%
    mutate(num_rainy_sun = num_rainy_sun + num_tagged_rainy_sun)

  return(count_sun)

}

# check for NA before using pbinom
utils_binom <- function(x, n, p){
  if(any(is.na(c(x, n, p))))
    return(NA)
  prob = pbinom(x, n, p)
}
