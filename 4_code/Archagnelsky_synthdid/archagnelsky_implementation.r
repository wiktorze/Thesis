rm(list = ls())
pacman::p_load(devtools, data.table, synthdid)
dt = fread("separated_treated_tracts/dt_treated_tract_17031080400.csv")
dt = dt[,c("census_t_1", "time_index", "total", "treat_post")]
setup = panel.matrices(dt)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat)

dt = fread("dt_treated_tract_17031080500.csv")
dt = dt[,c("census_t_1", "time_index", "total", "treat_post")]
setup = panel.matrices(dt)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat)


# now, perform a set of estimations for all the dt_treated_tract_*.csv files
# and save the results in a file
setwd("./2_intermediary/separated_treated_tracts")
file_names = list.files(pattern = "dt_treated_tract_.*csv")
# Get tract number from file_names
tracts = gsub("dt_treated_tract_(.*).csv", "\\1", file_names)
results = list()

for (i in 1:length(file_names)) {
    dt = fread(file_names[i])
    dt = dt[,c("census_t_1", "time_index", "crime_rate", "treat_post")]
    setup = panel.matrices(dt)
    tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
    se = sqrt(vcov(tau.hat, method='placebo'))
    tau.hat_num = as.numeric(tau.hat)
    results[[i]] = list(tract = tracts[i], est = tau.hat_num, tau_synth = tau.hat, error = se)
}

# save results in R format
saveRDS(results, "results.rds")

# Take 10 quarters after
for (i in 1:length(file_names)) {
    dt = fread(file_names[i])
    dt[, first_treat := min(time_index[treat_post == 1])]
    dt = dt[,c("census_t_1", "time_index", "crime_rate", "treat_post", "first_treat")]
    # take 7 months before treatment starts and 10 months after treatment starts
    ft = dt[treat_post == 1, first_treat][1]
    dt = dt[time_index <= ft + 10]
    dt = dt[,c("census_t_1", "time_index", "crime_rate", "treat_post")]
    setup = panel.matrices(dt)
    tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
    se = sqrt(vcov(tau.hat, method='placebo'))
    results[[i]] = list(tract = tracts[i], tau_synth = tau.hat, error = se)
}
saveRDS(results, "results_sdid_simple_10q.rds")
