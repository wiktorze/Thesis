### ADJUST FOR POPULATION ###
# If 1 unit demolished population is reduced by 1
rm(list=ls())
pacman::p_load(data.table)
crime_rate_q_trimmed = fread("crime_rates/crime_rate_q_trimmed.csv")
# once a unit is demolished, population is reduced by 2 in all periods after
crime_rate_q_trimmed[, adj_population := pop - cumsum(no_units * 1), by = .(census_t_1)] 

crime_rate_q_trimmed[, crime_rate_adj := fifelse(adj_population <= 0, 0, total/adj_population*1000)]
fwrite(crime_rate_q_trimmed, "crime_rate_q_trimmed_adj.csv")
