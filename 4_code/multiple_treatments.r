# This file modifies dt to analyze into multiple treatments for the same tract being separated with a unique id
# idea: take 7 quarters of pre-period data and 10 quarter of post-period data
# for units that are treated multiple times, take 7 quarters of pre-treatment and min(treatment_change, 10) of post-period data
# create new id column that combines census_t_1 and treat_change
rm(list=ls())
pacman::p_load(data.table)
dt = fread("./2_intermediary/dt_analyze_q.csv")

# check number of treated units
dt[treat_post == 1, .N, by = census_t_1]
# 42 treated
# check first_treat by census_t_1
dt[treat_post == 1, .(first_treat = min(time_index)), by = census_t_1]
# exclude last quarter
dt = dt[time_index != 49]

dt[, treat_change := cumsum(no_units > 0), by = census_t_1]
sum(dt$no_units)

# keep tracts with minimum 7 quarters of pre-treatment data
# so remove the first treatment if started before 7th quarter
# set no_units to 0 for the first treat_change
dt[(treat_change != 0 & time_index < 7), no_units := 0]
dt_est = dt
dt_est[, treat_change := cumsum(no_units > 0), by = census_t_1]
sum(dt_est$no_units)

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

# window for time_index - 7 pre-treatment and time_index + 10 post-treatment if treat_index >=10
# window for time_index - 7 pre-treatment and time_index + treat_index post-treatment if treat_index < 10
# only when treatment kicks in, so where treat_index == 1
dt_new = dt_est[treat_index == 1]
# for each row take the window from dt_est of time_index earliest to time_index + 10
l = list()
l_n = list()
i=24
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
    fwrite(dt_n2, paste0("./synthdid_dt/", dt_new[i, census_t_1], "_", dt_new[i, treat_change], ".csv"))
    l_n[[i]] = dt_n
    l[[i]] = dt_n2
}
# save l
#saveRDS(l, "./synthdid_dt/l.rds")
# for gsynth
dt_gsynth = unique(rbindlist(l))

saveRDS(dt_gsynth, "./gsynth_dt/dt_gsynth.rds")

# how many units in the analysis?
sum(unique(dt_gsynth[, .(census_t_1, no_units, time_index)])$no_units)
# 16,054