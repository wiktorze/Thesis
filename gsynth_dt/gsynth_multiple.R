rm(list=ls())
pacman::p_load(gsynth, data.table, panelView, ggplot2)
dt_analyze = readRDS("./gsynth_dt/dt_gsynth.rds")

panelview(total ~ treat_post, data = dt_analyze, 
          index = c("census_t_1","time_index"), pre.post = TRUE, 
          by.timing = TRUE, xlab = "Month", ylab = "Tracts", cex.axis.x = 2)

model <- gsynth(log_crime_rate ~ treat_post, 
               data = dt_analyze, index = c("census_t_1","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,5), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 7)

plot(model, type = "gap", xlim = c(-7, 11))

print(model)
# weight att by no_units
cumu1 <- cumuEff(model, cumu = TRUE, id = NULL, period = c(0,10))
cumu1
