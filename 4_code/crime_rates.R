# Crime Rates
rm(list=ls())
pacman::p_load(data.table, haven, stringr, sf, sp, parallel, readxl)
crime = fread("2_intermediary/crime_agg_units.csv")
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

# winsorize crime_rate, total, econ_crime, violent_crime, drug_crime and pop
crime_rate[, crime_rate_w := fifelse(crime_rate > quantile(crime_rate, 0.95), quantile(crime_rate, 0.95), crime_rate)]
crime_rate[, total_w := fifelse(total > quantile(total, 0.95), quantile(total, 0.95), total)]
crime_rate[, econ_crime_w := fifelse(econ_crime > quantile(econ_crime, 0.95), quantile(econ_crime, 0.95), econ_crime)]
crime_rate[, violent_crime_w := fifelse(violent_crime > quantile(violent_crime, 0.95), quantile(violent_crime, 0.95), violent_crime)]
crime_rate[, drug_crime_w := fifelse(drug_crime > quantile(drug_crime, 0.95), quantile(drug_crime, 0.95), drug_crime)]
crime_rate[, pop_w := fifelse(pop > q2, q2, pop)]

crime_rate[, crime_rate_w := fifelse(crime_rate < quantile(crime_rate, 0.05), quantile(crime_rate, 0.05), crime_rate)]
crime_rate[, total_w := fifelse(total < quantile(total, 0.05), quantile(total, 0.05), total)]
crime_rate[, econ_crime_w := fifelse(econ_crime < quantile(econ_crime, 0.05), quantile(econ_crime, 0.05), econ_crime)]
crime_rate[, violent_crime_w := fifelse(violent_crime < quantile(violent_crime, 0.05), quantile(violent_crime, 0.05), violent_crime)]
crime_rate[, drug_crime_w := fifelse(drug_crime < quantile(drug_crime, 0.05), quantile(drug_crime, 0.05), drug_crime)]
crime_rate[, pop_w := fifelse(pop < q1, q1, pop)]

crime_rate_q[, crime_rate_w := fifelse(crime_rate > quantile(crime_rate, 0.95), quantile(crime_rate, 0.95), crime_rate)]
crime_rate_q[, total_w := fifelse(total > quantile(total, 0.95), quantile(total, 0.95), total)]
crime_rate_q[, econ_crime_w := fifelse(econ_crime > quantile(econ_crime, 0.95), quantile(econ_crime, 0.95), econ_crime)]
crime_rate_q[, violent_crime_w := fifelse(violent_crime > quantile(violent_crime, 0.95), quantile(violent_crime, 0.95), violent_crime)]
crime_rate_q[, drug_crime_w := fifelse(drug_crime > quantile(drug_crime, 0.95), quantile(drug_crime, 0.95), drug_crime)]
crime_rate_q[, pop_w := fifelse(pop > q2, q2, pop)]

crime_rate_q[, crime_rate_w := fifelse(crime_rate < quantile(crime_rate, 0.05), quantile(crime_rate, 0.05), crime_rate)]
crime_rate_q[, total_w := fifelse(total < quantile(total, 0.05), quantile(total, 0.05), total)]
crime_rate_q[, econ_crime_w := fifelse(econ_crime < quantile(econ_crime, 0.05), quantile(econ_crime, 0.05), econ_crime)]
crime_rate_q[, violent_crime_w := fifelse(violent_crime < quantile(violent_crime, 0.05), quantile(violent_crime, 0.05), violent_crime)]
crime_rate_q[, drug_crime_w := fifelse(drug_crime < quantile(drug_crime, 0.05), quantile(drug_crime, 0.05), drug_crime)]
crime_rate_q[, pop_w := fifelse(pop < q1, q1, pop)]

# summary for total, pop
summary(crime_rate$total)
summary(crime_rate$pop)

summary(crime_rate_q$total)
summary(crime_rate_q$pop)

# log of crime rate
crime_rate[, log_crime_rate := log(crime_rate)]
crime_rate_q[, log_crime_rate := log(crime_rate)]

# log of total, econ_crime, violent_crime, drug_crime
crime_rate[, log_total := log(total)]
crime_rate[, log_econ_crime := log(econ_crime)]
crime_rate[, log_violent_crime := log(violent_crime)]
crime_rate[, log_drug_crime := log(drug_crime)]

crime_rate_q[, log_total := log(total)]
crime_rate_q[, log_econ_crime := log(econ_crime)]
crime_rate_q[, log_violent_crime := log(violent_crime)]
crime_rate_q[, log_drug_crime := log(drug_crime)]

# create first difference of log_crime_rate for each tract
# if month_year 1999-01-01 then diff_log_crime_rate = NA
crime_rate[, diff_log_crime_rate := log_crime_rate - shift(log_crime_rate, fill = 0), by = census_t_1]
crime_rate[month_year == "1999-01-01", diff_log_crime_rate := NA]
crime_rate_q[, diff_log_crime_rate := log_crime_rate - shift(log_crime_rate, fill = 0), by = census_t_1]
crime_rate_q[quarter == 1 & year.x == 1999, diff_log_crime_rate := NA]
fwrite(crime_rate, "./crime_rates/crime_rate.csv")
fwrite(crime_rate_q, "./crime_rates/crime_rate_q.csv")

# number of treated units - tracts where stock_units > 0
treated_units = crime_rate[stock_units > 0, .(no_units = sum(no_units)), by = census_t_1]
sum(treated_units$no_units)
# 42 as before