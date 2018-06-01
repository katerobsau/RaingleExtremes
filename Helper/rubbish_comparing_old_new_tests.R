# maximum as read from RNOAA and gotten through functions

# inputs
stn_id = "ASN00022000" #"ASN00010525"
text.type.large <- element_text(size = 12)
text.type.small <- element_text(size = 10)

# get station prcp
prcp_var <- meteo_pull_monitors(stn_id,
                                date_min = "1910-01-01",
                                date_max = "2010-01-01",
                                keep_flags = TRUE,
                                var = "PRCP")

# get max data
stn_prcp <- prcp_var %>% select(date, prcp) %>%
  mutate(block = get_block_index(dates = date, 1, 12))
max_data_raw = get_stn_max(stn_prcp, stn_id, 1, 12, rmax = 4)

# test them using the old function
max_data = max_data_raw %>%
  mutate(date_old = year(date)*10000 + month(date)*100 + day(date)) %>%
  filter(year(date) >= 1910 & year(date) <= 2010) %>%
  mutate(max = if_else(num_obs >= 365*0.9, prcp, NA_real_)) %>%
  filter(!is.na(max))

test_old = extremesUntaggedAccum(date = max_data$date_old, plot.output = TRUE)
test_new = extremes_untagged_test(dates = max_data$date)

# Binomial Distribution
n = length(max_data$date)
p = 1/7
E = n*p
# sd = 2*sqrt(n/7*(1- 1/7))
ci = c(0.025, 0.975)
qLow = qbinom(ci[1], n, p);
qUpp = qbinom(ci[2], n, p);
bar_plot <- ggplot(test_new, aes(x = wday, y = freq)) +
  geom_col(col = "lightblue", fill = "lightblue") +
  geom_hline(yintercept = c(qLow, E, qUpp), linetype = "dashed") +
  theme_bw() +
  xlab("Weekdays") +
  ylab("Count") +
  ggtitle(paste("Frequency of Days Extremes were Observed at", stn_id)) +
  theme_bw() +
  theme(legend.text = text.type.small,
        axis.text = text.type.small,
        plot.title = text.type.large,
        legend.title = text.type.large,
        axis.title = text.type.large)
bar_plot

load("~/Dropbox/Hard Drive/GitHub/RainfallExtremes/Package/RainfallExtremes/data/KingTest-ARDROSSAN_4Maxima.RData")

max_data1 = maxData1 %>%
  select(Year, Month, Day, Rainfall.amount..millimetres.) %>%
  filter(!is.na(Rainfall.amount..millimetres.)) %>%
  mutate(date = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
  rename(block = Year, max = Rainfall.amount..millimetres.) %>%
  select(date, block, max)


test1_old = extremesUntaggedAccum(date = maxData1$Date, plot.output = TRUE)
test1_new = extremes_untagged_test(dates = max_data1$date)

# Binomial Distribution
n = length(max_data1$date)
p = 1/7
E = n*p
# sd = 2*sqrt(n/7*(1- 1/7))
ci = c(0.025, 0.975)
qLow = qbinom(ci[1], n, p);
qUpp = qbinom(ci[2], n, p);
bar_plot <- ggplot(test1_new, aes(x = wday, y = freq)) +
  geom_col(col = "lightblue", fill = "lightblue") +
  geom_hline(yintercept = c(qLow, E, qUpp), linetype = "dashed") +
  theme_bw() +
  xlab("Weekdays") +
  ylab("Count") +
  ggtitle(paste("Frequency of Days Extremes were Observed at", stn_id)) +
  theme_bw() +
  theme(legend.text = text.type.small,
        axis.text = text.type.small,
        plot.title = text.type.large,
        legend.title = text.type.large,
        axis.title = text.type.large)
bar_plot
