rm(list=ls())
pacman::p_load(gsynth, data.table, panelView, ggplot2)
dt_analyze = readRDS("./4_code/gsynth_dt/dt_gsynth.rds")

panelview(total ~ treat_post, data = dt_analyze, 
          index = c("id", "time_index"), pre.post = TRUE, 
          by.timing = TRUE, xlab = "Month", ylab = "Tracts", cex.axis.x = 2)

model <- gsynth(crime_rate ~ treat_post, 
               data = dt_analyze, index = c("id","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,4), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 6)

plot(model, type = "gap", xlim = c(-6, 8))
# weight this to get the final result by no_units
no_units_t = dt_analyze[no_units>0,sum(no_units), by = id]
print(model)
# weight att by no_units
att = rep(NA, nrow(no_units_t))
att_w = rep(NA, nrow(no_units_t))
se = rep(NA, nrow(no_units_t))
i = 1
cumuEff(model, cumu = FALSE, id = no_units_t$id[i], period = c(0,8))
for(i in 1:nrow(no_units_t)){
    cum = cumuEff(model, cumu = FALSE, id = no_units_t$id[i], period = c(0,8))
    att[i] = cum$catt[8]
    att_w[i] = att[i] * no_units_t[i, V1]
    se[i] = cum$est.catt[8,2]
}
att_cum = sum(att_w, na.rm = TRUE) / sum(no_units_t$V1[!is.na(att_w)])
se_cum = sqrt(sum(se^2, na.rm = TRUE)) / sum(no_units_t$V1[!is.na(att_w)])
# do this for every period
# adjusted
model_adj <- gsynth(log_crime_rate_adj ~ treat_post, 
               data = dt_analyze, index = c("id","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,4), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 6)

plot(model_adj, type = "gap", xlim = c(-6, 10))
print(model_adj)

# econ crimes

# violent crimes