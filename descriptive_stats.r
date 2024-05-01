# Crime Rates
rm(list=ls())
pacman::p_load(data.table, haven, stringr, data.table, ggplot2, parallel, readxl, sf)
crime = fread("crime_agg_units.csv")
treated_units_before = crime[stock_units > 0, sum(no_units), by = census_t_1]
sum(treated_units_before$V1)
# View(crime)
# Read txt files including zeros
pop <- read.table("/Users/mac/Downloads/tract_pop.txt", header = FALSE, sep = ",", 
                   colClasses = c("character", "character", "character", "character", "character"))  # Adjust types as needed
pop = as.data.table(pop)
pop = pop[V1 == "17" & V2 == "031",]
setnames(pop, "V3", "census_tra")
setnames(pop, "V4", "pop")
setnames(pop, "V5", "center_lat")
setnames(pop, "V6", "center_long")
pop = pop[,-c(1,2)]
# turn pop to numeric
pop$pop = as.numeric(pop$pop)
pop$center_lat = as.numeric(pop$center_lat)
pop$center_long = as.numeric(pop$center_long)
View(pop)
summary(pop$pop)

tracts = st_read("/Users/mac/Documents/Thesis/Data/Boundaries - Census Tracts - 2000/geo_export_f3cf6886-99ef-450b-9003-4ce53146d95c.shp")
pop_tract = merge(tracts, pop, by = "census_tra")

pop_tract = pop_tract[, c("census_t_1", "pop")]
# plot tract by population with points of public housing

pop_tract = as.data.table(pop_tract)

# desriptive stats for pop
summary(pop_tract$pop)

q1 = quantile(pop_tract$pop, 0.05)
q2 = quantile(pop_tract$pop, 0.95)
# turn census_t_1 to character
pop_tract$census_t_1 = as.character(pop_tract$census_t_1)
crime$census_t_1 = as.character(crime$census_t_1)
# join pop_tract to crime on census_t_1
crime_rate = merge(crime, pop_tract, by = "census_t_1")[, -c("geometry")]

#recalculate total by adding econ_crime, violent_crime and drug_crime
crime_rate[, total := econ_crime + violent_crime + drug_crime]

# calculate crime rate in every 
crime_rate[, crime_rate := fifelse(pop != 0, total/pop*1000, 0)]

# Aggregate crimes by quarter
crime_rate[, month_year := as.Date(month_year, format = "%m/%d/%Y")]
crime_rate[, quarter := quarter(month_year)]
crime_rate_q = crime_rate[, .(total = sum(total), econ_crime = sum(econ_crime), violent_crime = sum(violent_crime), drug_crime = sum(drug_crime), no_units = sum(no_units), pop = first(pop)), by = c("census_t_1", "quarter", "year.x")]
crime_rate_q[, crime_rate := fifelse(pop != 0, total/pop*1000, 0)]
summary(crime_rate_q$crime_rate)
# stock units as cumsum of units over time
crime_rate_q[, stock_units := cumsum(no_units), by = census_t_1]

# create treat variable taking value 1 if stock_units > 0 for all time periods
crime_rate_q$treat_post = fifelse(crime$stock_units > 0, 1, 0)
crime_rate_q$treat <- ifelse(crime$census_t_1 %in% crime[treat_post == 1, census_t_1], 1, 0)

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
