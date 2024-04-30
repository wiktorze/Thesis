rm(list = ls())
pacman::p_load(devtools, data.table, synthdid)
results = readRDS("results.rds")
# create a list of tau_synth
tau_synth = lapply(results, function(x) x$tau_synth)

synthdid_plot(tau_synth)
synthdid_controls(tau_synth)
print(tau_synth[1])
