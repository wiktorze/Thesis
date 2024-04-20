rm(list=ls())
pacman::p_load(data.table)
dt = fread("dt_analyze.csv")

# Create a dataset for every treated unit with all the control units
dt_treated = dt[dt$treat==1,]
dt_control = dt[dt$treat==0,]

# Create a dataset for every treated tract with all the control tracts
tracts_treated = unique(dt_treated$census_t_1)
for (i in 1:length(tracts_treated)){
    dt_treated_tract = dt_treated[dt_treated$census_t_1==tracts_treated[i],]
    dt_treated_tract = rbind(dt_treated_tract,dt_control)
    fwrite(dt_treated_tract,paste0("dt_treated_tract_",tracts_treated[i],".csv"))
}