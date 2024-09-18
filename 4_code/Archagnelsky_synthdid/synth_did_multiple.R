# Synth_did for multiple treatments
rm(list = ls())
pacman::p_load(devtools, data.table, synthdid)
#devtools::install_github("synth-inference/synthdid")

# now, perform a set of estimations for all the dt_treated_tract_*.csv files
# and save the results in a file
setwd("./synthdid_dt/")
file_names = list.files(pattern = ".*csv")
# Get tract number from file_names
tracts = gsub("(.*).csv", "\\1", file_names)
results = list()
for (i in 1:length(file_names)) {
    dt = fread(file_names[i])
    # make sure time_index for non-treated is the same as treated
    dt = dt[time_index >= min(dt[treat == 1, time_index])]
    w = dt[treat_index == 1, no_units][nrow(dt[treat_index == 1])]
    stock_u = dt[treat_index == 1, stock_units][nrow(dt[treat_index == 1])]
    # stock_u last period
    stock_u_l = stock_u - w
    dt = dt[,c("census_t_1", "time_index", "log_crime_rate", "treat_post")]
    setup = panel.matrices(dt)
    tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
    se = sqrt(vcov(tau.hat, method='placebo'))
    tau.hat_num = as.numeric(tau.hat)
    results[[i]] = list(tract = tracts[i], est = tau.hat_num, 
    tau_synth = tau.hat, error = se, w = w, stock_u = stock_u, stock_u_l = stock_u_l)
}

# save results in R format
saveRDS(results, "../Archagnelsky_synthdid/results_multiple.rds")
