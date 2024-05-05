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
pdf("./3_results/Figures/gsynth_dt_het.pdf" )
plot(model, type = "gap", xlim = c(-6, 10))
dev.off()
# weight this to get the final result by no_units
no_units_t = dt_analyze[no_units>0,sum(no_units), by = id]
print(model)
# weight att by no_units
att = rep(NA, nrow(no_units_t))
att_w = rep(NA, nrow(no_units_t))
se = rep(NA, nrow(no_units_t))
for(i in 1:nrow(no_units_t)){
    cum = cumuEff(model, cumu = FALSE, id = no_units_t$id[i], period = c(0,6))
    att[i] = cum$catt[6]
    att_w[i] = att[i] * no_units_t[i, V1]
    se_w[i] = cum$est.catt[6,2] * no_units_t[i, V1]
}
att_cum = sum(att_w, na.rm = TRUE) / sum(no_units_t$V1[!is.na(att_w)])
se_cum = sqrt(sum(se^2, na.rm = TRUE)/ sum(no_units_t$V1[!is.na(se_w)]))
print(att_cum)
print(se_cum)
# do this for every period
# adjusted
View(dt_analyze)
model_adj <- gsynth(crime_rate_adj ~ treat_post, 
               data = dt_analyze, index = c("id","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,4), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 6)
pdf("./3_results/Figures/gsynth_dt_het_adj.pdf" )
plot(model_adj, type = "gap", xlim = c(-6, 10))
dev.off()
# weight att by no_units
att = rep(NA, nrow(no_units_t))
att_w = rep(NA, nrow(no_units_t))
se = rep(NA, nrow(no_units_t))
for(i in 1:nrow(no_units_t)){
    cum = cumuEff(model_adj, cumu = FALSE, id = no_units_t$id[i], period = c(0,10))
    att[i] = cum$catt[10]
    att_w[i] = att[i] * no_units_t[i, V1]
    se[i] = cum$est.catt[10,2]
}
att_cum = sum(att_w, na.rm = TRUE) / sum(no_units_t$V1[!is.na(att_w)])
se_cum = sqrt(sum(se^2, na.rm = TRUE)/120)
print(att_cum)
print(se_cum)
cumuEff(model, cumu = FALSE, id = no_units_t$id[i], period = c(0,6))
cumuEff(model_adj, `cumu` = FALSE, id = no_units_t$id[i], period = c(0,6))

model$att
model_adj$att
# econ crimes

# violent crimes