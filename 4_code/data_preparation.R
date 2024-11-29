rm(list=ls())
pacman::p_load(data.table, haven, stringr, sf, sp, parallel, ggplot2)


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
units_merged <- merge(units, ph_tract, by = "demo_id")
# 17550
sum(units_merged$units, na.rm = TRUE)
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

pop <- fread("/Users/mac/Documents/Thesis/Data/nhgis_tract_2000_2019_popest_17/nhgis_tract_popest_2000_2019_17.csv")
pop[, GEOID := as.character(GEOID)]
# Group by tract and year, sum population
pop_general = pop[, .(
    pop_2000 = round(sum(ESTIMATE_2000)),
    pop_2001 = round(sum(ESTIMATE_2001)),
    pop_2002 = round(sum(ESTIMATE_2002)),
    pop_2003 = round(sum(ESTIMATE_2003)),
    pop_2004 = round(sum(ESTIMATE_2004)),
    pop_2005 = round(sum(ESTIMATE_2005)),
    pop_2006 = round(sum(ESTIMATE_2006)),
    pop_2007 = round(sum(ESTIMATE_2007)),
    pop_2008 = round(sum(ESTIMATE_2008)),
    pop_2009 = round(sum(ESTIMATE_2009)),
    pop_2010 = round(sum(ESTIMATE_2010))
), by = GEOID]

# to long
pop_general_long = melt(pop_general, id.vars = "GEOID", variable.name = "year", value.name = "population")

# subtract "pop_" from year
pop_general_long[, year := as.numeric(str_remove(year, "pop_"))]
# rename GEOID to geoid10
setnames(pop_general_long, "GEOID", "geoid10")

tracts = tracts[, c("geoid10", "tractce10", "geometry")]
pop_tract = merge(tracts, pop_general, by.x = "geoid10", by.y = "GEOID")

# plot tract by population with points of public housing
ggplot() + 
geom_sf(data = pop_tract, aes(fill = pop_2000)) + 
geom_sf(data = units_merged_sf, pch = 20, col = "red") +
theme_minimal()

# To each demolished unit, get the geoid10
units_merged_tract = st_join(units_merged_sf, tracts, join = st_intersects)

# clean
units_merged_tract = units_merged_tract[, c("demo_id", "property.x", "units", "demo_date", "demo_start", "demo_end", "demo_closure", "demo_eviction", "INTPTLAT00", "INTPTLON00", "geoid10", "CTIDFP00", "geometry")]
# rename columns: demo_id, property, demo_date, demo_start, demo_end, demo_closure, demo_eviction, project_lat, project_long, geoid10, geoid00, geometry
colnames(units_merged_tract) = c("demo_id", "property", "units", "demo_date", "demo_start", "demo_end", "demo_closure", "demo_eviction", "project_lat", "project_long", "geoid10", "geoid00", "geometry")

units_per_tract = as.data.table(units_merged_tract)[, .(no_units = sum(units)), by = c("geoid10", "demo_closure")]
units_per_tract[, year := year(demo_closure)]
units_per_tract[, month := month(demo_closure)]
units_per_tract = units_per_tract[, .(no_units = sum(no_units)), by = c("geoid10", "year", "month")]
# Now, create a new data.table with all the tracts and dates in tracts sf and the number of demolished units
# for each tract and date
# First, create a data.table with all the tracts and dates
tracts_dt = as.data.table(tracts)[, c("geoid10")]

# For each tract, expand it for years from 1999 to 2010 and create a year field
tracts_dt = tracts_dt[rep(seq_len(nrow(tracts_dt)), each = 13), .(geoid10, year = 1999:2011)]
# Now, do the same for months from 1 to 12
tracts_dt = tracts_dt[rep(seq_len(nrow(tracts_dt)), each = 12), .(geoid10, year, month = 1:12)]

# Based on the year and month, create a new field with the month and year
tracts_dt[, month_year := as.Date(paste(year, month, "01", sep = "/"), format = "%Y/%m/%d")]

# Now, add units from units_per_tract to tracts_dt
tracts_dt_new = merge(tracts_dt, units_per_tract, by = c("geoid10", "year", "month"), all.x = T)

sum(tracts_dt_new$no_units, na.rm = TRUE)

# Replace NAs with 0
tracts_dt_new[is.na(no_units), no_units := 0]

# Now, compute the stock of demolished units for each tract and month_year in time
# so, for each tract, compute the cumulative sum of demolished units
tracts_dt_new[, stock_units := cumsum(no_units), by = geoid10]

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
blocks = unique(crime_clean[, .(block, crime_lat, crime_long)])
block_points = st_as_sf(blocks, coords = c("crime_long", "crime_lat"), crs = st_crs(tracts))

# Match tracts to blocks
plot(tracts[, c("geoid10", "geometry")])
plot(block_points, add = TRUE)
tracts_block <- st_join(tracts, block_points, join = st_intersects)
crime_clean = merge(crime_clean, tracts_block, by = "block")

# Aggregate crimes by month, year, census_t_1 and sum total, econ_crime, violent_crime and drug_crime
crime_agg = crime_clean[, lapply(.SD, sum), by = .(monthofyear, year, geoid10), .SDcols = c("total", "econ_crime", "violent_crime", "drug_crime")]
crime_agg[, month_year := as.Date(paste("01", monthofyear, year, sep = "/"), format = "%d/%m/%Y")]

# create 3-month moving average for every geoid10
crime_agg[, total_ma3 := frollmean(total, 3, fill = NA), by = geoid10]
# Write the crime_agg
fwrite(crime_agg, "./2_intermediary/crime_agg_tract.csv")

# Join number of units demolished to the crime_agg
crime_agg_units = merge(crime_agg, tracts_dt_new[, .(geoid10, month_year, no_units, stock_units)], by = c("geoid10", "month_year"))

# Check how many tracts have stock_units>0
length(unique(crime_agg_units[geoid10 %in% crime_agg_units[stock_units > 0, geoid10], geoid10]))
# 38

# add population by year
crime_agg_units = merge(crime_agg_units, pop_general_long, by = c("geoid10", "year"), all.x = TRUE)

fwrite(crime_agg_units, "./2_intermediary/crime_agg_units.csv")
