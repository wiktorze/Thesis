rm(list=ls())
pacman::p_load(gsynth, data.table, panelView, ggplot2, augsynth, ggrepel)
#remotes::install_github("ebenmichael/augsynth")

dt_analyze = readRDS("./4_code/gsynth_dt/dt_gsynth.rds")

ppool_syn <- multisynth(crime_rate ~ treat_post, id, time_index, dt_analyze)

ppool_syn_sum <- summary(ppool_syn)
plot(ppool_syn_sum, levels = "Average")

panelview(total ~ treat_post, data = dt_analyze, 
          index = c("id", "time_index"), pre.post = TRUE, 
          by.timing = TRUE, xlab = "Month", ylab = "Tracts", cex.axis.x = 2)

model <- gsynth(crime_rate ~ treat_post, 
               data = dt_analyze, index = c("id","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,4), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 6)
dt_treated = dt_analyze[treat == 1 & no_units>0, .(id, census_t_1, no_units, time_index)]
dt_treated = dt_treated[, .(no_units = first(no_units)), by = .(census_t_1, time_index)]
# take census names from model (before _)
tracts_model = unique(gsub("_.*", "", model$id.tr))
dt_treated_count = dt_treated[census_t_1 %in% tracts_model]
sum(dt_treated_count$no_units)
pdf("./3_results/Figures/gsynth_dt_het_8q.pdf" )
plot(model, type = "gap", xlim = c(-8, 10))
dev.off()
# weight this to get the final result by no_units
no_units_t = dt_analyze[no_units>0 & id %in% model$id.tr,sum(no_units), by = id]
print(model)
# weight att by no_units
att = rep(NA, nrow(no_units_t))
att_w = rep(NA, nrow(no_units_t))
se = rep(NA, nrow(no_units_t))
for(i in 1:nrow(no_units_t)){
    cum = cumuEff(model, cumu = FALSE, id = no_units_t$id[i], period = c(0,10))
    att[i] = cum$catt[10]
    att_w[i] = att[i] * no_units_t[i, V1]
    se[i] = cum$est.catt[10,2] * no_units_t[i, V1]
}
att_cum = sum(att_w, na.rm = TRUE) / sum(no_units_t$V1[!is.na(att_w)])
se_cum = sqrt(sum(se^2, na.rm = TRUE)/ sum(no_units_t$V1[!is.na(se)])) / 120
print(att_cum)
print(se_cum)
# do this for every period
# adjusted
#View(dt_analyze)
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
    att[i] = cum$catt[6]
    att_w[i] = att[i] * no_units_t[i, V1]
    se[i] = cum$est.catt[6,2] * no_units_t[i, V1]
}
att_cum = sum(att_w, na.rm = TRUE) / sum(no_units_t$V1[!is.na(att_w)])
se_cum = sqrt(sum(se^2, na.rm = TRUE)/ sum(no_units_t$V1[!is.na(se)])) / 120
print(att_cum)
print(se_cum)
cumuEff(model, cumu = FALSE, id = no_units_t$id[i], period = c(0,6))
cumuEff(model_adj, `cumu` = FALSE, id = no_units_t$id[i], period = c(0,6))

model$att
model_adj$att

# skip small demolitions - assign treatment only once stock_units > 75
dt_analyze_big = readRDS("./4_code/gsynth_dt/dt_gsynth_big.rds")

model_big <- gsynth(crime_rate_adj ~ treat_post, 
               data = dt_analyze_big, index = c("id","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,4), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 6)
dt_treated = dt_analyze_big[treat == 1 & no_units>75, .(id, census_t_1, no_units, time_index)]
dt_treated = dt_treated[, .(no_units = first(no_units)), by = .(census_t_1, time_index)]
dt_treated = unique(dt_treated)
sum(dt_treated$no_units)
# take census names from model (before _)
tracts_model = unique(gsub("_.*", "", model$id.tr))
dt_treated_count = dt_treated[census_t_1 %in% tracts_model]
sum(dt_treated_count$no_units)

pdf("./3_results/Figures/gsynth_dt_het_big.pdf" )
plot(model_big, type = "gap", xlim = c(-6, 10))
dev.off()
# weight att by no_units
no_units_t = dt_analyze_big[no_units>75,sum(no_units), by = id]

sum(no_units_t$V1)
att = rep(NA, nrow(no_units_t))
att_w = rep(NA, nrow(no_units_t))
se = rep(NA, nrow(no_units_t))
for(i in 1:nrow(no_units_t)){
    cum = cumuEff(model_big, cumu = FALSE, id = no_units_t$id[i], period = c(0,10))
    att[i] = cum$catt[10]
    att_w[i] = att[i] * no_units_t[i, V1]
    se[i] = cum$est.catt[10,2] * no_units_t[i, V1]
}

att_cum = sum(att_w, na.rm = TRUE) / sum(no_units_t$V1[!is.na(att_w)])
se_cum = sqrt(sum(se^2, na.rm = TRUE)/ sum(no_units_t$V1[!is.na(se)])) / i
print(att_cum)
print(se_cum)

# econ crimes
dt_analyze[, crime_rate_econ := fifelse(pop != 0, econ_crime/pop*1000, 0)]
summary(dt_analyze$crime_rate_econ)
model_econ <- gsynth(crime_rate_econ ~ treat_post, 
               data = dt_analyze, index = c("id","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,4), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 6)
pdf("./3_results/Figures/gsynth_dt_het_econ.pdf" )
plot(model_econ, type = "gap", xlim = c(-6, 10))
dev.off()
model_econ$est.att

# violent crimes
dt_analyze[, crime_rate_violent := fifelse(pop != 0, violent_crime/pop*1000, 0)]
summary(dt_analyze$crime_rate_violent)
model_violent <- gsynth(crime_rate_violent ~ treat_post, 
               data = dt_analyze, index = c("id","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,4), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 6)
pdf("./3_results/Figures/gsynth_dt_het_violent.pdf" )
plot(model_violent, type = "gap", xlim = c(-6, 10))
dev.off()
model_violent$est.att

# drug crimes
dt_analyze[, crime_rate_drug := fifelse(pop != 0, drug_crime/pop*1000, 0)]
summary(dt_analyze$crime_rate_drug)
model_drug <- gsynth(crime_rate_drug ~ treat_post, 
               data = dt_analyze, index = c("id","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,4), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 6)
pdf("./3_results/Figures/gsynth_dt_het_drug.pdf")
plot(model_drug, type = "gap", xlim = c(-6, 10))
dev.off()
model_drug$est.att
