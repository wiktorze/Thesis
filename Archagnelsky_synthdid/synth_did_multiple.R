# Synth_did for multiple treatments
rm(list = ls())
pacman::p_load(devtools, data.table, synthdid)

dt = fread("synthdid_dt/17031080400_1.csv")
dt = dt[,c("census_t_1", "time_index", "crime_rate", "treat_post")]
setup = panel.matrices(dt)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='jackknife'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat)


# now, perform a set of estimations for all the dt_treated_tract_*.csv files
# and save the results in a file
setwd("./synthdid_dt/")
file_names = list.files(pattern = ".*csv")
# Get tract number from file_names
tracts = gsub("(.*).csv", "\\1", file_names)
results = list()

for (i in 1:length(file_names)) {
    dt = fread(file_names[i])
    w = dt[treat_index == 1, no_units][-1]
    dt = dt[,c("census_t_1", "time_index", "crime_rate", "treat_post")]
    setup = panel.matrices(dt)
    tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
    se = sqrt(vcov(tau.hat, method='placebo'))
    tau.hat_num = as.numeric(tau.hat)
    results[[i]] = list(tract = tracts[i], est = tau.hat_num, tau_synth = tau.hat, error = se, w = w)
}

# save results in R format
saveRDS(results, "../Archagnelsky_synthdid/results_multiple.rds")

# weight our results by units demolished in the treatment
mean_eff = mean(sapply(results, function(x) x$est * x$w))