rm(list=ls())
pacman::p_load(data.table, haven, stringr, sf, sp)


# Load the data -----------------------------------------------------------

pop = as.data.table(read_dta("/Users/mac/Documents/Thesis/100512-V1/Replication/blockpop.dta"))

crime = as.data.table(read_dta("/Users/mac/Documents/Thesis/100512-V1/Replication/crimeblocks.dta"))

housing = as.data.table(read_dta("/Users/mac/Documents/Thesis/100512-V1/Replication/Public_Housing.dta"))

units = as.data.table(read_dta("/Users/mac/Documents/Thesis/100512-V1/Replication/CHAdemo_units.dta"))

demo = as.data.table(read_dta("/Users/mac/Documents/Thesis/100512-V1/Replication/CHAdemo.dta"))

housing_tract = as.data.table(read_dta("/Users/mac/Documents/Thesis/100512-V1/Replication/PH_CensusTract_Xwalk.dta"))

# Convert date in units
units[, demo_date := as.Date(demo_date, origin = "1960-01-01")]
units[, demo_start := as.Date(demo_start, origin = "1960-01-01")]
units[, demo_end := as.Date(demo_end, origin = "1960-01-01")]
units = units[demo_start >= "1999-01-01"]

# Merge units with housing to get lat long
units_merged = merge(units, housing, on = demo_id)

# census tracts - read shp file
tracts = st_read("/Users/mac/Documents/Thesis/Data/Boundaries - Census Tracts - 2010/geo_export_24a3592a-4039-4f19-afd3-987209b1f813.shp")

# subtract 60 days from demo_start and name it demo_closure
units_merged[, demo_closure := demo_start - 60]
# eviction
units_merged[, demo_eviction := demo_start - 150]

# Match units demolished with tracts using project_lat, project_long and tracts geometry
units_merged_sf = st_as_sf(units_merged, coords = c("project_long", "project_lat"), crs = st_crs(tracts))

# Plot them
plot(st_geometry(tracts), border="#aaaaaa")
plot(st_geometry(units_merged_sf), add = TRUE, pch = 20, col = "red")

# To each demolished unit, get the tractce10
units_merged_tract = st_join(units_merged_sf, tracts, join = st_intersects)
units_merged_sf$tractce10 <- units_merged_tract$tractce10
# 42 tracts for units_merged_sf

# Take demo_closure as the treatment time - extract month and year from demo_closure
units_merged_dt = as.data.table(units_merged_sf)
units_merged_dt[, demo_month := month(demo_closure)]
units_merged_dt[, demo_year := year(demo_closure)]
# New field with month and year
units_merged_dt[, month_year := as.Date(paste("01", demo_month, demo_year, sep = "/"), format = "%d/%m/%Y")]
# Count units demolished per tractce and month_year
units_per_tract = units_merged_dt[, .(no_units = sum(units)), by = c("tractce10", "month_year")]

# Now, create a new data.table with all the tracts and dates in tracts sf and the number of demolished units
# for each tract and date
# First, create a data.table with all the tracts and dates
tracts_dt = as.data.table(tracts)[, c("tractce10")]
# For each tract, expand it for years from 1999 to 2010 and create a year field
tracts_dt = tracts_dt[rep(seq_len(nrow(tracts_dt)), each = 12), .(tractce10, year = 1999:2010)]
# Now, do the same for months from 1 to 12
tracts_dt = tracts_dt[rep(seq_len(nrow(tracts_dt)), each = 12), .(tractce10, year, month = 1:12)]

# Based on the year and month, create a new field with the month and year
tracts_dt[, month_year := as.Date(paste("01", month, year, sep = "/"), format = "%d/%m/%Y")]

# Now, add units from units_per_tract to tracts_dt
tracts_dt = merge(tracts_dt, units_per_tract, by = c("tractce10", "month_year"), all.x = TRUE) 

# Replace NAs with 0
tracts_dt[is.na(no_units), no_units := 0]

# Now, compute the stock of demolished units for each tract and month_year in time
# so, for each tract, compute the cumulative sum of demolished units
tracts_dt[, stock_units := cumsum(no_units), by = tractce10]
# Show tractce10 == 080400 sorted by month_year
# View(tracts_dt[tractce10 == "080400", .(tractce10, month_year, no_units, stock_units)][order(month_year)])
# Works!

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
monthofyear, year, total, econ_crime, violent_crime, drug_crime)]

# Now, match all the crimes to census tracts
# So, for each crime, have tractce10 from tracts
# First, create a centroid out of crime_lat and crime_long
crime_points = st_as_sf(crime_clean, coords = c("crime_long", "crime_lat"), crs = st_crs(tracts))

# Join tracts to crimes
crime_tracts = st_join(crime_points, tracts, join = st_intersects)
crime_clean$tractce10 = crime_tracts$tractce10
crime_clean = crime_clean[!is.na(tractce10)]

# Aggregate crimes by month, year, tractce10 and sum total, econ_crime, violent_crime and drug_crime
crime_agg = crime_clean[, lapply(.SD, sum), by = .(monthofyear, year, tractce10), .SDcols = c("total", "econ_crime", "violent_crime", "drug_crime")]
crime_agg[, month_year := as.Date(paste("01", monthofyear, year, sep = "/"), format = "%d/%m/%Y")]
# Write the crime_agg
fwrite(crime_agg, "crime_agg_tract.csv")

# Join number of units demolished to the crime_agg
crime_agg_units = merge(crime_agg, tracts_dt, by = c("tractce10", "month_year"))
fwrite(crime_agg_units, "crime_agg_units.csv")
