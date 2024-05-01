rm(list=ls())
pacman::p_load(data.table, haven, stringr, sf, sp, parallel)


# Load the data -----------------------------------------------------------

pop = as.data.table(read_dta("/Users/mac/Documents/Thesis/100512-V1/Replication/blockpop.dta"))

crime = as.data.table(read_dta("/Users/mac/Documents/Thesis/100512-V1/Replication/crimeblocks.dta"))
ph_tract = fread("/Users/mac/Documents/Thesis/100512-V1/Replication/PH_CensusTract_Xwalk.csv")
#rename project_la to project_lat, project_lo to project_long
setnames(ph_tract, c("project_la", "project_lo"), c("project_lat", "project_long"))

ph = fread("/Users/mac/Documents/Thesis/100512-V1/Replication/public_housing_geocode.csv")
housing = as.data.table(read_dta("/Users/mac/Documents/Thesis/100512-V1/Replication/Public_Housing.dta"))

units = as.data.table(read_dta("/Users/mac/Documents/Thesis/100512-V1/Replication/CHAdemo_units.dta"))

housing_tract = as.data.table(read_dta("/Users/mac/Documents/Thesis/100512-V1/Replication/PH_CensusTract_Xwalk.dta"))

# Convert date in units
units[, demo_date := as.Date(demo_date, origin = "1960-01-01")]
units[, demo_start := as.Date(demo_start, origin = "1960-01-01")]
units[, demo_end := as.Date(demo_end, origin = "1960-01-01")]
units = units[demo_start >= "1999-01-01"]

# Merge units with housing to get lat long
units_merged = merge(units, ph_tract, on = demo_id)

# census tracts - read shp file
tracts = st_read("/Users/mac/Documents/Thesis/Data/Boundaries - Census Tracts - 2000/geo_export_f3cf6886-99ef-450b-9003-4ce53146d95c.shp")

# subtract 60 days from demo_start and name it demo_closure
units_merged[, demo_closure := demo_start - 60]
# eviction
units_merged[, demo_eviction := demo_start - 150]

# Match units demolished with tracts using project_lat, project_long and tracts geometry
units_merged_sf = st_as_sf(units_merged, coords = c("project_long", "project_lat"), crs = st_crs(tracts))

# Plot them
plot(st_geometry(tracts), border="#aaaaaa")
plot(st_geometry(units_merged_sf), add = TRUE, pch = 20, col = "red")

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

pop_tract = merge(tracts, pop, by = "census_tra")

pop_tract = pop_tract[, c("census_t_1", "pop")]

# plot tract by population with points of public housing
ggplot() + 
geom_sf(data = pop_tract, aes(fill = pop)) + 
geom_sf(data = units_merged_sf, pch = 20, col = "red") +
theme_minimal()

# To each demolished unit, get the census_t_1
units_merged_tract = st_join(units_merged_sf, tracts, join = st_intersects)
units_merged_sf$census_t_1 <- units_merged_tract$census_t_1
# count census_t_1
length(unique(units_merged_sf$census_t_1))

# Take demo_closure as the treatment time - extract month and year from demo_closure
units_merged_dt = as.data.table(units_merged_sf)
units_merged_dt[, demo_month := month(demo_closure)]
units_merged_dt[, demo_year := year(demo_closure)]
units_merged_dt[, demo_month_e := month(demo_eviction)]
units_merged_dt[, demo_year_e := year(demo_eviction)]
# New field with month and year
units_merged_dt[, month_year := as.Date(paste("01", demo_month, demo_year, sep = "/"), format = "%d/%m/%Y")]
units_merged_dt[, month_year_e := as.Date(paste("01", demo_month_e, demo_year_e, sep = "/"), format = "%d/%m/%Y")]
# Count units demolished per tractce and month_year
units_per_tract = units_merged_dt[, .(no_units = sum(units)), by = c("census_t_1", "month_year")]
units_per_tract_e = units_merged_dt[, .(no_units = sum(units)), by = c("census_t_1", "month_year_e")]

# Now, create a new data.table with all the tracts and dates in tracts sf and the number of demolished units
# for each tract and date
# First, create a data.table with all the tracts and dates
tracts_dt = as.data.table(tracts)[, c("census_t_1")]
# unique and sort tracts_dt
tracts_dt = unique(tracts_dt)
# are all units_per_tract$census_t_1 in tracts_dt$census_t_1?
unique(units_per_tract$census_t_1) %in% tracts_dt$census_t_1
# yes
# For each tract, expand it for years from 1999 to 2010 and create a year field
tracts_dt = tracts_dt[rep(seq_len(nrow(tracts_dt)), each = 13), .(census_t_1, year = 1999:2011)]
# Now, do the same for months from 1 to 12
tracts_dt = tracts_dt[rep(seq_len(nrow(tracts_dt)), each = 12), .(census_t_1, year, month = 1:12)]

# Based on the year and month, create a new field with the month and year
tracts_dt[, month_year := as.Date(paste(year, month, "01", sep = "/"), format = "%Y/%m/%d")]
tracts_dt[, month_year := as.character(month_year)]
units_per_tract[, month_year := as.character(month_year)]
# Now, add units from units_per_tract to tracts_dt
tracts_dt_e = merge(tracts_dt, units_per_tract_e, by.x = c("census_t_1", "month_year"), by.y = c("census_t_1", "month_year_e"), all.x = TRUE)
tracts_dt = merge(tracts_dt, units_per_tract, by = c("census_t_1", "month_year"), all.x = TRUE) 
sum(tracts_dt_new$no_units, na.rm = TRUE)
# Replace NAs with 0
tracts_dt[is.na(no_units), no_units := 0]
tracts_dt_e[is.na(no_units), no_units := 0]

# Now, compute the stock of demolished units for each tract and month_year in time
# so, for each tract, compute the cumulative sum of demolished units
tracts_dt[, stock_units := cumsum(no_units), by = census_t_1]
tracts_dt_e[, stock_units := cumsum(no_units), by = census_t_1]

### CRIME ###
# Classify crime by type - econ crime, violent crime and drug crime
# econ crime is burglary, theft, car_theft, robbery
# violent crime is murder, assault, rape, arson
# drug crime is drugs
crime[, econ_crime := fifelse(burglary!=0 | theft != 0 | car_theft != 0 | robbery != 0,
                              rowSums(crime[, c("burglary", "theft", "car_theft", "robbery")], na.rm = TRUE),
                              0)] 
crime[, violent_crime := fifelse(murder != 0 | assault != 0 | rape != 0 | arson != 0, 
rowSums(crime[,.(murder, assault, rape, arson)], na.rm = TRUE), 
0)]
crime[, drug_crime := drugs]
crime_clean = crime[!is.na(crime_lat) & !is.na(crime_long)][, .(crime_lat, crime_long, 
monthofyear, year, total, econ_crime, violent_crime, drug_crime, block)]

# Now, match all the crimes to census tracts
# So, for each crime, have census_t_1 from tracts
# First, create a centroid out of crime_lat and crime_long
crime_points = st_as_sf(crime_clean, coords = c("crime_long", "crime_lat"), crs = st_crs(tracts))

# Join tracts to crimes
crime_tracts = st_join(crime_points, tracts, join = st_intersects)
crime_clean$census_t_1 = crime_tracts$census_t_1
crime_clean = crime_clean[!is.na(census_t_1)]


# Aggregate crimes by month, year, census_t_1 and sum total, econ_crime, violent_crime and drug_crime
crime_agg = crime_clean[, lapply(.SD, sum), by = .(monthofyear, year, census_t_1), .SDcols = c("total", "econ_crime", "violent_crime", "drug_crime")]
crime_agg[, month_year := as.Date(paste("01", monthofyear, year, sep = "/"), format = "%d/%m/%Y")]

# Write the crime_agg
fwrite(crime_agg, "crime_agg_tract.csv")

# Join number of units demolished to the crime_agg
crime_agg[,month_year := as.character(month_year)]
crime_agg_units = merge(crime_agg, tracts_dt, by = c("census_t_1", "month_year"))
# Check how many tracts have stock_units>0
length(unique(crime_agg_units[census_t_1 %in% crime_agg_units[stock_units > 0, census_t_1], census_t_1]))
# still 42
fwrite(crime_agg_units, "crime_agg_units.csv")
