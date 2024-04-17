# Crime Rates
rm(list=ls())
pacman::p_load(data.table, haven, stringr, sf, sp, parallel, readxl)
crime = fread("crime_agg_units.csv")
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
tracts = st_read("/Users/mac/Documents/Thesis/Data/Boundaries - Census Tracts - 2000/geo_export_f3cf6886-99ef-450b-9003-4ce53146d95c.shp")
pop_tract = merge(tracts, pop, by = "census_tra")
# turn pop to numeric
pop_tract$pop = as.numeric(pop_tract$pop)
pop_tract$center_lat = as.numeric(pop_tract$center_lat)
pop_tract$center_long = as.numeric(pop_tract$center_long)

pop_tract = pop_tract[, c("census_t_1", "pop")]

pop_tract = as.data.table(pop_tract)
# winsorize pop at 1% and 99%
pop_tract[, pop := fifelse(pop < quantile(pop, 0.1), quantile(pop, 0.01),
                       fifelse(pop > quantile(pop, 0.9), quantile(pop, 0.99), pop))]
# turn census_t_1 to character
pop_tract$census_t_1 = as.character(pop_tract$census_t_1)
crime$census_t_1 = as.character(crime$census_t_1)
# join pop_tract to crime on census_t_1
crime_rate = merge(crime, pop_tract, by = "census_t_1")[, -c("geometry")]

# calculate crime rate in every 
crime_rate[, crime_rate := total/pop*1000]

fwrite(crime_rate, "crime_rate.csv")
