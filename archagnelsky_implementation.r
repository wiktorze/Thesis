rm(list = ls())
pacman::p_load(devtools, data.table, synthdid)
dt = fread("dt_treated_tract_17031080400.csv")
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
file_names = list.files(pattern = "dt_treated_tract_.*csv")
results = data.table()
for (i in 1:length(file_names)) {
    dt = fread(file_names[i])
    dt = dt[,c("census_t_1", "time_index", "total", "treat_post")]
    setup = panel.matrices(dt)
    tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
    se = sqrt(vcov(tau.hat, method='placebo'))
    fwrite(data.table(tau = tau.hat, sec = se), file = paste0("results_", file_names[i], ".csv")) 
}
