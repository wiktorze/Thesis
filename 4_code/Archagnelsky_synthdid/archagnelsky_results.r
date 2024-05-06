rm(list = ls())
pacman::p_load(devtools, data.table, synthdid)
results = readRDS("./3_results/results_sdid_simple_10q_adj.rds")
# create a list of tau_synth
tau_synth = lapply(results, function(x) x$tau_synth)

pdf("./3_results/Figures/sdid_simple_10q.pdf")
synthdid_plot(tau_synth[1])
dev.off()
for(i in 1:35){
print(tau_synth[i])
}
print(summary(tau_synth[[1]]))

# mean of the tau_synth
mean_tau_synth = mean(sapply(tau_synth, function(x) as.numeric(x)))
print(mean_tau_synth)
# - 4.87
se = lapply(results, function(x) x$error)
# mean error - square them, add them up, divide by the number of observations, take the square root
se = unlist(se)
mean_error = sqrt(sum(se^2)/length(se))
print(mean_error)
# 4.61
### multiple treatments
results_multiple = readRDS("./3_results/results_multiple_normal_rate.rds")
tau_synth_mtp = lapply(results_multiple, function(x) x$tau_synth)

# mean of the tau_synth
mean_tau_synth_mtp = mean(sapply(tau_synth_mtp, function(x) as.numeric(x)))
se_mtp = lapply(results_multiple, function(x) x$error)
# mean error - square them, add them up, divide by the number of observations, take the square root
se_mtp = unlist(se_mtp)
mean_error_mtp = sqrt(sum(se_mtp^2)/length(se_mtp))

# weight att by no_units
dt_analyze = readRDS("./4_code/gsynth_dt/dt_gsynth.rds")
no_units_t = dt_analyze[no_units>0,sum(no_units), by = id]
att = rep(NA, nrow(no_units_t))
att_w = rep(NA, nrow(no_units_t))
se = rep(NA, nrow(no_units_t))
for(i in 1:nrow(no_units_t)){
    att[i] = tau_synth_mtp[[i]]
    att_w[i] = att[i] * no_units_t[i, V1]
    se[i] = se_mtp[i] * no_units_t[i, V1]
}
att_cum = sum(att_w, na.rm = TRUE) / sum(no_units_t$V1[!is.na(att_w)])
se_cum = sqrt(sum(se^2, na.rm = TRUE)/ sum(no_units_t$V1[!is.na(se)])) / 120
print(att_cum)
print(se_cum)

# adjusted rate
results_multiple = readRDS("./3_results/results_multiple_adj_rate.rds")
tau_synth_mtp = lapply(results_multiple, function(x) x$tau_synth)

# mean of the tau_synth
mean_tau_synth_mtp = mean(sapply(tau_synth_mtp, function(x) as.numeric(x)))
se_mtp = lapply(results_multiple, function(x) x$error)
# mean error - square them, add them up, divide by the number of observations, take the square root
se_mtp = unlist(se_mtp)
mean_error_mtp = sqrt(sum(se_mtp^2)/length(se_mtp))

# weight att by no_units
dt_analyze = readRDS("./4_code/gsynth_dt/dt_gsynth.rds")
no_units_t = dt_analyze[no_units>0,sum(no_units), by = id]
att = rep(NA, nrow(no_units_t))
att_w = rep(NA, nrow(no_units_t))
se = rep(NA, nrow(no_units_t))
for(i in 1:nrow(no_units_t)){
    att[i] = tau_synth_mtp[[i]]
    att_w[i] = att[i] * no_units_t[i, V1]
    se[i] = se_mtp[i] * no_units_t[i, V1]
}
att_cum = sum(att_w, na.rm = TRUE) / sum(no_units_t$V1[!is.na(att_w)])
se_cum = sqrt(sum(se^2, na.rm = TRUE)/ sum(no_units_t$V1[!is.na(se)])) / 120
print(att_cum)
print(se_cum)
