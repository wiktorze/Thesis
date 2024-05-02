# Crime Rates
rm(list=ls())
pacman::p_load(data.table, haven, stringr, data.table, ggplot2, parallel, readxl, sf, viridis, scales)
crime = fread("./2_intermediary/crime_agg_units.csv")
treated_units_before = crime[stock_units > 0, sum(no_units), by = census_t_1]
sum(treated_units_before$V1)
# no demolitions - how many times no_units>0
nrow(crime[no_units > 0])
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
#View(pop)
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
crime_rate$treat_post = fifelse(crime_rate$stock_units > 0, 1, 0)
crime_rate$treat <- ifelse(crime_rate$census_t_1 %in% crime_rate[treat_post == 1, census_t_1], 1, 0)

# by quarter
crime_rate_q$treat_post = fifelse(crime_rate_q$stock_units > 0, 1, 0)
crime_rate_q$treat <- ifelse(crime_rate_q$census_t_1 %in% crime_rate_q[treat_post == 1, census_t_1], 1, 0)


# plot mean crime number for treated and untreated tracts by month_year
mean_treated = crime_rate[treat == 1, .(mean_total = mean(total)), by = month_year]
mean_untreated = crime_rate[treat == 0, .(mean_total = mean(total)), by = month_year]

# plot mean of total crimes for treated and untreated tracts by month_year on the same graph
ggplot() +
  geom_line(data = mean_treated, aes(x = month_year, y = mean_total, color = "Treated")) +
  geom_line(data = mean_untreated, aes(x = month_year, y = mean_total, color = "Untreated")) +
  labs(title = "Mean total crimes for treated and untreated tracts by month_year",
       x = "Month Year",
       y = "Mean Total Crimes") +
  theme_minimal()

### UNITS DEMOLSIHED
ph_tract = fread("/Users/mac/Documents/Thesis/100512-V1/Replication/PH_CensusTract_Xwalk.csv")
setnames(ph_tract, c("project_la", "project_lo"), c("project_lat", "project_long"))
units = as.data.table(read_dta("/Users/mac/Documents/Thesis/100512-V1/Replication/CHAdemo_units.dta"))
# Convert date in units
units[, demo_date := as.Date(demo_date, origin = "1960-01-01")]
units[, demo_start := as.Date(demo_start, origin = "1960-01-01")]
units[, demo_end := as.Date(demo_end, origin = "1960-01-01")]
units = units[demo_start >= "1999-01-01"]

# Merge units with housing to get lat long
units_merged = merge(units, ph_tract, on = demo_id)
# subtract 60 days from demo_start and name it demo_closure
units_merged[, demo_closure := demo_start - 60]
# eviction
units_merged[, demo_eviction := demo_start - 150]

# Match units demolished with tracts using project_lat, project_long and tracts geometry
units_merged_sf = st_as_sf(units_merged, coords = c("project_long", "project_lat"), crs = st_crs(tracts))
units_merged[, year := year(demo_start)]

### CENSUS
# Economic characteristics
census_econ = fread("/Users/mac/Documents/Thesis/Data/census_data/productDownload_2024-03-19T130535/DECENNIALDPSF32000.DP3-Data.csv")
names = as.character(census_econ[2,])
setnames(census_econ, names(census_econ), names)
# columns: 6, 98:2:116, 117, 171, 195
subset_econ <- census_econ[-c(1:4), c(1, 2, 6, 12, 117, 171, 195)]

cnames = c(
  "id",
  "area_name",
  "labor_force",
  "unemployed",
  "median_income",
  "poverty_fam",
  "poverty_ind")
setnames(subset_econ, colnames(subset_econ), cnames)
# Extract tract from id: all numbers after "US"
subset_econ$census_t_1 <- str_sub(subset_econ$id, 10)
subset_econ[, c("labor_force", "unemployed", "median_income", "poverty_fam", "poverty_ind") := lapply(.SD, as.numeric), .SDcols = c("labor_force", "unemployed", "median_income", "poverty_fam", "poverty_ind")]

# merge with tracts
tracts_min = tracts[, c("census_t_1", "geometry")]
econ_tract = merge(tracts_min, subset_econ, by = "census_t_1")
# plot tracts by median income, poverty_fam, unemployment side by side
# use colorful scale to differentiate easily
par(mfrow = c(1, 3))
ggplot() +
  geom_sf(data = econ_tract, aes(fill = median_income)) +
  geom_sf(data = units_merged_sf, pch = 20, col = "red") +
  labs(title = "",
       fill = "Median Income") +
  theme_minimal() +
  scale_fill_viridis() 
# add points of public housing
ggplot() +
  geom_sf(data = econ_tract, aes(fill = poverty_fam)) +
  geom_sf(data = units_merged_sf, pch = 20, col = "red") +
  labs(title = "",
       fill = "Family Poverty Rate") +
  theme_minimal() +
  scale_fill_viridis() 

ggplot() +
  geom_sf(data = econ_tract, aes(fill = unemployed)) +
  geom_sf(data = units_merged_sf, pch = 20, col = "red") +
  labs(title = "",
       fill = "Unemployment Rate") +
  theme_minimal() +
  scale_fill_viridis() 

# Crimes by tract
crime_tract_00 = crime_rate[year.x == 2000, .(total = sum(total)), by = census_t_1]
# join with tract
crime_tract_00 = merge(tracts_min, crime_tract_00, by = "census_t_1")
ggplot() +
  geom_sf(data = crime_tract_00, aes(fill = total)) +
  geom_sf(data = units_merged_sf, pch = 20, col = "red") +
  labs(title = "Total crimes by tract in 2000",
       fill = "Total Crimes") +
  theme_minimal() +
  scale_fill_viridis()

# winsorized data that I'm using
dt_analyze = fread("./2_intermediary/dt_analyze.csv")
dt_analyze_00 = dt_analyze[year.x == 2000, .(crime_rate = mean(crime_rate)), by = census_t_1]
# join with tract
dt_analyze_00 = merge(tracts_min, dt_analyze_00, by = "census_t_1")
# plot crime rate by tract
ggplot() +
  geom_sf(data = dt_analyze_00, aes(fill = crime_rate)) +
  geom_sf(data = units_merged_sf, pch = 20, col = "red") +
  labs(title = "Crime rate by tract in 2000",
       fill = "Crime Rate") +
  theme_minimal() +
  scale_fill_viridis()

# Crimes by year (bar) vs units demolished by year (line)
# use 2 different scales on left and right
crime_agg = crime_rate[, .(total = sum(total)), by = year.x][, year := year.x][year!=2011]
units_agg = crime_rate[, .(no_units = sum(stock_units)), by = year.x][, year := year.x][year!=2011]
ggplot() +
  geom_bar(data = crime_agg, aes(x = year, y = total), stat = "identity") +
  geom_line(data = units_agg, aes(x = year, y = no_units), color = "red") +
  labs(title = "Total crimes by year vs units demolished by year",
       x = "Year",
       y = "Total Crimes") +
  theme_minimal() +
  scale_y_continuous(name = "Total Crimes", 
                     sec.axis = sec_axis(~ . * max(crime_agg$total)/max(units_agg$no_units), 
                                         name = "Units Demolished", 
                                         breaks = pretty_breaks())) 

# Number of units demolished by tract
units_tract = units_merged[, .(no_units = sum(units)), by = CTIDFP00]
print(units_tract[order(no_units, decreasing = TRUE)])
