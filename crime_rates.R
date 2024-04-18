# Crime Rates
rm(list=ls())
pacman::p_load(data.table, haven, stringr, sf, sp, parallel, readxl)
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
# trim 5% and 95% of pop
q1 = quantile(pop$pop, 0.05)
q2 = quantile(pop$pop, 0.95)
pop = pop[pop > q1 & pop < q2]

tracts = st_read("/Users/mac/Documents/Thesis/Data/Boundaries - Census Tracts - 2000/geo_export_f3cf6886-99ef-450b-9003-4ce53146d95c.shp")
pop_tract = merge(tracts, pop, by = "census_tra")

pop_tract = pop_tract[, c("census_t_1", "pop")]

pop_tract = as.data.table(pop_tract)

# desriptive stats for pop
summary(pop_tract$pop)
# turn census_t_1 to character
pop_tract$census_t_1 = as.character(pop_tract$census_t_1)
crime$census_t_1 = as.character(crime$census_t_1)
# join pop_tract to crime on census_t_1
crime_rate = merge(crime, pop_tract, by = "census_t_1")[, -c("geometry")]

# calculate crime rate in every 
crime_rate[, crime_rate := fifelse(pop != 0, total/pop*1000, 0)]

fwrite(crime_rate, "crime_rate.csv")
# number of treated units - tracts where stock_units > 0
treated_units = crime_rate[stock_units > 0, .(no_units = sum(no_units)), by = census_t_1]
sum(treated_units$no_units)
# 39 vs 41 at the beginning

# Aggregate crimes by quarter
crime_rate[, month_year := as.Date(month_year, format = "%m/%d/%Y")]
crime_rate[, quarter := quarter(month_year)]
crime_rate
crime_rate_q = crime_rate[, .(total = sum(total), econ_crime = sum(econ_crime), violent_crime = sum(violent_crime), drug_crime = sum(drug_crime), no_units = sum(no_units), pop = first(pop)), by = c("census_t_1", "quarter", "year.x")]
crime_rate_q[, crime_rate := fifelse(pop != 0, total/pop*1000, 0)]
summary(crime_rate_q$crime_rate)
# stock units as cumsum of units over time
crime_rate_q[, stock_units := cumsum(no_units), by = census_t_1]

fwrite(crime_rate_q, "crime_rate_q.csv")
summary(crime_rate_q$crime_rate)
# drop tracts that have crime rate above 100
tracts_to_drop = unique(crime_rate_q[crime_rate > 100, census_t_1])
crime_rate_q_trimmed = crime_rate_q[!census_t_1 %in% tracts_to_drop]
summary(crime_rate_q_trimmed$crime_rate)
# number of treated units - tracts where stock_units > 0
treated_units = crime_rate_q_trimmed[stock_units > 0, .(no_units = sum(no_units)), by = census_t_1]
sum(treated_units$no_units)
# lost another 2 treated units
fwrite(crime_rate_q_trimmed, "crime_rate_q_trimmed.csv")
