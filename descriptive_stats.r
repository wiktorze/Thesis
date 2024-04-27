# DESCRIPTIVE STATS
rm(list=ls())
pacman::p_load(data.table, ggplot2)
crime = fread("crime_rates/crime_rate.csv")

# create treat variable taking value 1 if stock_units > 0 for all time periods
crime$treat_post = fifelse(crime$stock_units > 0, 1, 0)
crime$treat <- ifelse(crime$census_t_1 %in% crime[treat_post == 1, census_t_1], 1, 0)

# plot mean crime rate for treated and untreated tracts by month_year
mean_treated = crime[treat == 1, .(mean_total = mean(total)), by = month_year]
mean_untreated = crime[treat == 0, .(mean_total = mean(total)), by = month_year]

# plot mean of total crimes for treated and untreated tracts by month_year on the same graph
ggplot() +
  geom_line(data = mean_treated, aes(x = month_year, y = mean_total, color = "Treated")) +
  geom_line(data = mean_untreated, aes(x = month_year, y = mean_total, color = "Untreated")) +
  labs(title = "Mean total crimes for treated and untreated tracts by month_year",
       x = "Month Year",
       y = "Mean Total Crimes") +
  theme_minimal()

# plot log of mean crime rate for treated and untreated tracts by month_year
mean_treated = crime[treat == 1, .(mean_log_total = mean(log_total)), by = month_year]
mean_untreated = crime[treat == 0, .(mean_log_total = mean(log_total)), by = month_year]
ggplot() +
  geom_line(data = mean_treated, aes(x = month_year, y = mean_log_total, color = "Treated")) +
  geom_line(data = mean_untreated, aes(x = month_year, y = mean_log_total, color = "Untreated")) +
  labs(title = "Mean total crimes for treated and untreated tracts by month_year",
       x = "Month Year",
       y = "Mean Total Crimes") +
  theme_minimal()

# calculate first differences of log to get growth rates
crime[, delta := c(NA, diff(log_total)), by = census_t_1]
View(crime)

# plot growth rates for treated and untreated tracts by month_year
mean_treated = crime[treat == 1, .(mean_delta = mean(delta, na.rm = TRUE)), by = month_year]
mean_untreated = crime[treat == 0, .(mean_delta = mean(delta, na.rm = TRUE)), by = month_year]

ggplot() +
  geom_line(data = mean_treated, aes(x = month_year, y = mean_delta, color = "Treated")) +
  geom_line(data = mean_untreated, aes(x = month_year, y = mean_delta, color = "Untreated")) +
  labs(title = "Mean growth rates for treated and untreated tracts by month_year",
       x = "Month Year",
       y = "Mean Growth Rates") +
  theme_minimal()

# extract month_year
month_year = unique(crime$month_year)
# create a new column which is a row index in month_year
crime[, time_index := match(month_year, crime$month_year)]
# save the data to analyze
fwrite(crime, "dt_ready/crime_rate_analyze.csv")

# quarterly data
crime_q = fread("crime_rates/crime_rate_q.csv")
crime_q[, q_year := paste(quarter, year.x, sep = "/")]
quarter_year = unique(crime_q$q_year)
# create a new column which is a row index in q_year
crime_q[, time_index := match(q_year, crime_q$q_year)]
# Treated variable
crime_q$treat_post = fifelse(crime_q$stock_units > 0, 1, 0)
crime_q$treat <- ifelse(crime_q$census_t_1 %in% crime_q[treat_post == 1, census_t_1], 1, 0)

# log of total
crime_q[, log_total := log(total)]
crime_q[, delta := c(NA, diff(log_total)), by = census_t_1]


fwrite(crime_q, "dt_ready/crime_rate_q.csv")

# do the same with trimmed data
crime_q_trimmed = fread("crime_rates/crime_rate_q_trimmed.csv")
crime_q_trimmed[, q_year := paste(quarter, year.x, sep = "/")]
quarter_year = unique(crime_q_trimmed$q_year)
# create a new column which is a row index in q_year
crime_q_trimmed[, time_index := match(q_year, crime_q_trimmed$q_year)]
# Treated variable
crime_q_trimmed$treat_post = fifelse(crime_q_trimmed$stock_units > 0, 1, 0)
crime_q_trimmed$treat <- ifelse(crime_q_trimmed$census_t_1 %in% crime_q_trimmed[treat_post == 1, census_t_1], 1, 0)
# log of total
crime_q_trimmed[, log_total := log(total)]
crime_q_trimmed[, delta := c(NA, diff(log_total)), by = census_t_1]
# write
fwrite(crime_q_trimmed, "dt_ready/crime_rate_q_trimmed.csv")
