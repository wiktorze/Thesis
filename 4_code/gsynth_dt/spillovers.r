rm(list = ls())
pacman::p_load(data.table, gsynth, sf, haven)
results_multiple = readRDS("./4_code/gsynth_dt/dt_gsynth.rds")

# make the tracts which border with the treated tracts treated
# exclude the originally treated tracts from the analysis
tracts = st_read("/Users/mac/Documents/Thesis/Data/Boundaries - Census Tracts - 2000/geo_export_f3cf6886-99ef-450b-9003-4ce53146d95c.shp")
tracts_min = tracts[, c("census_t_1", "geometry")]

neighbors <- st_touches(tracts_min, # first 
                        tracts_min, # second
                        sparse = T)


tracts_treated = unique(results_multiple[treat==1, census_t_1])
# for each treated tract, list a neighbours of the tract
tracts_spill = list()
for(i in 1:length(tracts_treated)){
    idx = which(tracts_min$census_t_1 == tracts_treated[i])
    nbr = neighbors[idx][[1]]
    tracts_spill$neighbors[[i]] = tracts_min$census_t_1[nbr]
    tracts_spill$treated[[i]] = tracts_treated[i]
}
# assign treatment to neighbours of treated tracts
# assign also no_units to the neighbours
# build a new treatment dt
treat_org = unique(results_multiple[treat == 1 & no_units > 0, .(id, census_t_1, time_index, no_units)])
# if more than 1 row for same id, take the one with the highest time_index
treat_org = treat_org[order(-time_index), .(id, census_t_1, time_index, no_units)]
treat_org = treat_org[!duplicated(id),]
treat_org = treat_org[order(census_t_1, time_index)]
# now create a new dt with only neighbours being treated
treat_spill = data.table()
for(i in 1:length(tracts_spill$neighbors)){
    for(j in 1:length(tracts_spill$neighbors[[i]])){
        treat_spill = rbind(treat_spill, data.table(
                                                     census_t_1 = tracts_spill$neighbors[[i]][j], 
                                                     time_index = treat_org[census_t_1 == tracts_spill$treated[[i]], time_index],
                                                     no_units = treat_org[census_t_1 == tracts_spill$treated[[i]], no_units],
                                                     demo_id = treat_org[census_t_1 == tracts_spill$treated[[i]], id]))
    }
}
# if the more than 1 row for the same census_t_1,
# sum no_units
treat_spill = treat_spill[, .(no_units = sum(no_units)), by = .(census_t_1, time_index)]
View(treat_spill[order(census_t_1, time_index)])
length(unique(treat_spill$census_t_1))
### SOMETHING WRONG HERE - number of neighbors is different later

# build the dt_spill
# exclude the treated tracts from the analysis
dt_spill = results_multiple[treat == 0, -c("id", "no_units")][, .(census_t_1, time_index, total, pop, adj_population, crime_rate, crime_rate_adj)]
# assign the treatment to the neighbours
dt_spill[, census_t_1 := as.character(census_t_1)]
dt_spill = merge(dt_spill, treat_spill, by = c("census_t_1", "time_index"), all.x = TRUE)
# some neighbours are the ex-treated

# turn NA to 0 no_units
dt_spill[is.na(no_units), no_units := 0]

# create stock_units - a cumulative sum of no_units in a tract
dt_spill = dt_spill[order(census_t_1, time_index)]
dt_spill[, stock_units := cumsum(no_units), by = census_t_1]
View(dt_spill[stock_units > 0])

# create treat-post
dt_spill[, treat_post := fifelse(stock_units > 0, 1, 0)]
dt_spill$treat <- ifelse(dt_spill$census_t_1 %in% dt_spill[treat_post == 1, census_t_1], 1, 0)


# first_treat
View(dt_spill[treat_post == 1, .(first_treat = min(time_index)), by = census_t_1])
length(unique(dt_spill[treat_post == 1, census_t_1]))
# exclude last quarter
dt_spill = dt_spill[time_index != 49]

dt_spill[, treat_change := cumsum(no_units > 0), by = census_t_1]

# keep tracts with minimum 7 quarters of pre-treatment data
# so remove the first treatment if started before 7th quarter
# set no_units to 0 for the first treat_change
dt_spill[(treat_change != 0 & time_index < 7), no_units := 0]
dt_est = dt_spill
dt_est[, treat_change := cumsum(no_units > 0), by = census_t_1]

# 31 treated
dt_est[, treat_index := 0]
# create a column which starts indexes the treatment until it changes
# so if the treat_change > 0 and stays the same, the index is 1, 2, 3, etc.
# if treat_change changes, the index is 1 again
# change dt_est to dataframe
dt_est = as.data.frame(dt_est)
for(i in 1:nrow(dt_est)){
    if(dt_est[i, "treat_change"] == 0){
        dt_est[i, "treat_index"] = 0
    } else if(dt_est[i, "treat_change"] - dt_est[i-1, "treat_change"] == 1){
        dt_est[i, "treat_index"] = 1
    } else {
        dt_est[i, "treat_index"] = dt_est[i-1, "treat_index"] + 1
    }
}
setDT(dt_est)
# create a list of maximum of treat_index by census_t_1, treat_change
dt_est[, treat_index_max := max(treat_index), by = .(census_t_1, treat_change)]

dt_new = dt_est[treat_index == 1]
# for each row take the window from dt_est of time_index earliest to time_index + 10
l = list()
l_n = list()
for(i in 1:nrow(dt_new)){
    dt_n = dt_est[census_t_1 == dt_new[i, census_t_1] & time_index <= min(dt_new[i, time_index] + 10, dt_new[i, time_index] + dt_new[i, treat_index_max] - 1)]
    # now, treat_post is different - it's when the last treatment_change occurs
    dt_n[, treat_post := fifelse(time_index >= dt_new[i, time_index], 1, 0)]
    dt_n[, id := paste0(census_t_1, "_", i)]
    # bind non-treated
    dt_nt = dt_est[treat == 0 & time_index <= min(dt_new[i, time_index] + 10, dt_new[i, time_index] + dt_new[i, treat_index_max] - 1)]
    dt_nt[, id := as.character(census_t_1)]
    dt_n2 = rbind(dt_n, dt_nt)
    # write dt_n2 to csv
    #fwrite(dt_n2, paste0("./synthdid_dt/", dt_new[i, census_t_1], "_", dt_new[i, treat_change], ".csv"))
    l_n[[i]] = dt_n
    l[[i]] = dt_n2
}
# for gsynth
dt_gsynth = unique(rbindlist(l))

saveRDS(dt_gsynth, "./4_code/gsynth_dt/dt_gsynth_spillover.rds")
View(dt_gsynth[treat==1])