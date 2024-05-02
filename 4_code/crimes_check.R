crimes = fread("/Users/mac/Downloads/Crimes_-_2001_to_Present.csv")
# filter crimes before 2010
crimes = crimes[Year <= 2010]
# crime by tract
crimes = crimes[!is.na(Longitude) & !is.na(Latitude)]
crimes_points = st_as_sf(crimes, coords = c("Longitude", "Latitude"), crs = st_crs(tracts))

crimes$tract = st_join(crimes_points, tracts, join = st_intersects)$tractce10

crime_tract = crime[, .(total = sum(total), econ_crime = sum(econ_crime), violent_crime = sum(violent_crime), drug_crime = sum(drug_crime)), by = tract]
crime_tract2 = crimes[, .(total = .N), by = tract]
View(crime_tract)
View(crime_tract2)
