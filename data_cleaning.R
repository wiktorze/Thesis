rm(list=ls())
pacman::p_load(data.table, haven, stringr, sf, sp)

# Load the data -----------------------------------------------------------

pop = as.data.table(read_dta("/Users/mac/Documents/Thesis/100512-V1/Replication/blockpop.dta"))

crime = as.data.table(read_dta("/Users/mac/Documents/Thesis/100512-V1/Replication/crimeblocks.dta"))

blocks = as.data.table(read_dta("/Users/mac/Documents/Thesis/100512-V1/Replication/latlongblocks.dta"))

housing = as.data.table(read_dta("/Users/mac/Documents/Thesis/100512-V1/Replication/Public_Housing.dta"))

units = as.data.table(read_dta("/Users/mac/Documents/Thesis/100512-V1/Replication/CHAdemo_units.dta"))

demo = as.data.table(read_dta("/Users/mac/Documents/Thesis/100512-V1/Replication/CHAdemo.dta"))

housing_tract = as.data.table(read_dta("/Users/mac/Documents/Thesis/100512-V1/Replication/PH_CensusTract_Xwalk.dta"))
tracts = unique(housing_tract$tract)


# Census data -------------------------------------------------------------
# Population
census_demography = as.data.table(fread("/Users/mac/Documents/Thesis/Data/census_data/productDownload_2024-03-19T130535/DECENNIALDPSF22000.DP1-Data.csv"))
subset_dt <- census_demography[V2 %like% "Census Tract" & V4  == "Total population", .(V1, V2, V4, V5)]
cnames = c(
  "id",
  "area_name",
  "field",
  "tot_pop"
)
setnames(subset_dt, colnames(subset_dt), cnames)

# Extract the desired portion of the id
subset_dt$tract <- str_sub(subset_dt$id, -6)
subset_dt$tract <- gsub("^0+", "", subset_dt$tract)
subset_dt$tract <- as.numeric(subset_dt$tract)

# Check if we have info on all tracts of demoslished buildings - YES
all(tracts %in% subset_dt$tract)

# Social characteristics
census_social = fread("/Users/mac/Documents/Thesis/Data/census_data/productDownload_2024-03-19T130535/DECENNIALDPSF32000.DP2-Data.csv")
names = as.character(census_social[2,])
setnames(census_social, names(census_social), names)
subset_social <- census_social[, c(1,2, seq(18,30,2))]
# 3-4 - no educ, 5-6 high school, 7-8-9 - degree
subset_social[, c(3:9) := lapply(.SD, as.numeric), .SDcols = c(3:9)]
subset_social[, educ_none := .SD[[3]] + .SD[[4]]]
subset_social[, educ_hs := .SD[[5]] + .SD[[6]]]
subset_social[, educ_deg := .SD[[7]] + .SD[[8]] + .SD[[9]]]
subset_social = subset_social[, c(1,2,10,11,12)]
cnames = c(
  "id",
  "area_name",
  "educ_none",
  "educ_hs",
  "educ_deg")
setnames(subset_social, colnames(subset_social), cnames)
subset_social = subset_social[area_name %like% "Census Tract",]

# Extract the desired portion of the id
subset_social$tract <- str_sub(subset_social$id, -6)
subset_social$tract <- gsub("^0+", "", subset_social$tract)
subset_social$tract <- as.numeric(subset_social$tract)

# Check if we have info on all tracts of demolished buildings - YES
all(tracts %in% subset_social$tract)


# Economic characteristics
census_econ = fread("/Users/mac/Documents/Thesis/Data/census_data/productDownload_2024-03-19T130535/DECENNIALDPSF32000.DP3-Data.csv")
names = as.character(census_econ[2,])
setnames(census_econ, names(census_econ), names)
# columns: 6, 98:2:116, 117, 171, 195
subset_econ <- census_econ[, c(1, 2, 6, 12, 117, 171, 195)]

cnames = c(
  "id",
  "area_name",
  "labor_force",
  "unemployed",
  "median_income",
  "poverty_fam",
  "poverty_ind")
setnames(subset_econ, colnames(subset_econ), cnames)

# Housing characteristics
census_housing = fread("/Users/mac/Documents/Thesis/Data/census_data/productDownload_2024-03-19T130535/DECENNIALDPSF32000.DP4-Data.csv")
names = as.character(census_housing[2,])
setnames(census_housing, names(census_housing), names)
# columns: 57, 187
subset_housing <- census_housing[, c(1, 2, 57, 187)]

cnames = c(
  "id",
  "area_name",
  "median_rooms",
  "median_rent")
setnames(subset_housing, colnames(subset_housing), cnames)

# Merging -----------------------------------------------------------------

# Convert date in units
units[, demo_date := as.Date(demo_date, origin = "1960-01-01")]
units[, demo_start := as.Date(demo_start, origin = "1960-01-01")]
units[, demo_end := as.Date(demo_end, origin = "1960-01-01")]

# Merge units with housing to get lat long
units_merged = merge(units, housing, on = demo_id)

# blocks - read shp file
blocks = st_read("/Users/mac/Documents/Thesis/Data/Boundaries - Census Blocks - 2010/geo_export_6b335484-a783-41d6-b02b-408cc5dbd4ad.shp")
blocks = blocks[, c("tract_bloc", "geometry")]

# Spatial match (intersection) between chicago_buf centroid and blocks
invalid_blocks = !st_is_valid(blocks)
valid_blocks = blocks[!invalid_blocks,]

# FIGURE 1 - where the demolitions occurred
plot(st_geometry(valid_blocks))
points(units_merged$project_long, units_merged$project_lat, col = "red", pch = 10)

# Create a buffer around each centroid
# Create an empty list to store the buffers
buffers <- vector(mode = "list", length = nrow(units_merged)) 

# Iterate over each row
for (i in 1:nrow(units_merged)) {
  # Create a point
  point <- st_sfc(st_point(c(units_merged$project_long[i], units_merged$project_lat[i])), crs = st_crs(blocks))
  # Create a buffer around the point
  buffers[[i]] <- st_buffer(point, dist = 4828.03) 
}
# Create a vector of the intersection of each buffer with the blocks
chicago_sel = vector(mode = "list", length = nrow(units_merged))
block_names = vector(mode = "list", length = nrow(units_merged))
for (i in 1:nrow(units_merged)) {
  chicago_sel[[i]] = st_filter(valid_blocks, buffers[[i]])
  # Retrieve blockce10 from valid_blocks that are intersected with the buffer
  block_names[[i]] = chicago_sel[[i]]$tract_bloc
}

length(unique(unlist(block_names)))
# 19,191 blocks for >75 units
# 23,977 unique blocks for all

# add blocks_unique to the plot
plot(st_geometry(valid_blocks[tract_bloc %in% blocks_unique,], add = T, col = "red"))

# Blocks to analyze
blocks_unique = unique(unlist(block_names))
blocks_analyze = valid_blocks[valid_blocks$tract_bloc %in% blocks_unique,]

# FIGURE 2 - blocks to analyze
plot(st_geometry(valid_blocks), border="#aaaaaa")
plot(st_geometry(blocks_analyze), add = T, col = "red")

# Merge blocks with crime data
# Catch the points from latitude and longitude of crimes
# Which lie inside the blocks_analyze
# Skip NAs in crime_lat, crime_long
crime_clean = crime[!is.na(crime_lat) & !is.na(crime_long),]
nrow(crime) - nrow(crime_clean)
# 146 crimes skipped
# Turn them into points
# To optimize, filter crime points based on bounding boxes of blocks_analyze
# Then, intersect the points with the blocks
blocks_box = st_bbox(blocks_analyze)
crime_clean = crime_clean[crime_long > blocks_box[1] & crime_long < blocks_box[3] & crime_lat > blocks_box[2] & crime_lat < blocks_box[4],]
crime_points = st_as_sf(crime_clean, coords = c("crime_long", "crime_lat"), crs = st_crs(blocks_analyze))
# Get the crimes that are in the blocks
crime_analyze = st_join(crime_points, blocks_analyze, join = st_intersects)

# classify the crimes - crime_bloc without geometry column
crime_bloc = as.data.table(crime_analyze)
crime_bloc = crime_bloc[!is.na(tract_bloc)]
crime_bloc = crime_bloc[, -c("geometry")]
crime_bloc[, econ_crime := fifelse(burglary!=0 | theft != 0 | car_theft != 0 | robbery != 0,
                              rowSums(crime_bloc[, c("burglary", "theft", "car_theft", "robbery")], na.rm = TRUE),
                              0)] 
crime_bloc[, violent_crime := fifelse(murder != 0 | assault != 0 | rape != 0 | arson != 0, 
rowSums(crime_bloc[,.(murder, assault, rape, arson)], na.rm = TRUE), 
0)]
crime_bloc[, drug_crime := drugs]

# Aggregate crimes
crime_agg_bloc = crime_bloc[, lapply(.SD, sum), by = .(monthofyear, year, tract_bloc), .SDcols = c("total", "econ_crime", "violent_crime", "drug_crime")]


# Save the blocks and crimes
st_write(blocks_analyze, "blocks_analyze.shp")
# Save crime_agg_bloc
fwrite(crime_agg_bloc, "crime_agg_bloc.csv")

