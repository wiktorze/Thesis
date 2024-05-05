rm(list=ls())
pacman::p_load(data.table)
dt = fread("./2_intermediary/dt_analyze_q.csv")

# Create a dataset for every treated unit with all the control units
dt_treated = dt[dt$treat==1,]
dt_control = dt[dt$treat==0,]
dt_early = unique(dt_treated[time_index < 7 & treat_post == 1, census_t_1])
dt_treated = dt_treated[!(census_t_1 %in% dt_early)]
# Create a dataset for every treated tract with all the control tracts
tracts_treated = unique(dt_treated$census_t_1)
for (i in 1:length(tracts_treated)){
    dt_treated_tract = dt_treated[dt_treated$census_t_1==tracts_treated[i],]
    dt_treated_tract = rbind(dt_treated_tract,dt_control)
    fwrite(dt_treated_tract,paste0("./2_intermediary/separated_treated_tracts/dt_treated_tract_",tracts_treated[i],".csv"))
}