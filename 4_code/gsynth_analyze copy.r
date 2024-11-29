rm(list=ls())
pacman::p_load(gsynth, data.table, panelView, ggplot2)

dt_analyze = fread("2_intermediary/crime_rate.csv")


panelview(total ~ treat_post, data = dt_analyze, 
          index = c("census_t_1","time_index"), pre.post = TRUE, 
          by.timing = TRUE, xlab = "Month", ylab = "Tracts", cex.axis.x = 2)
dev.off()

panelview(total ~ treat_post, data = dt_analyze, 
          index = c("census_t_1","time_index"), type = "outcome", 
          main = "Public housing demolitions and total crimes", 
          by.group = TRUE)

model <- gsynth(total ~ treat_post, 
               data = dt_analyze, index = c("census_t_1","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,5), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 7)
plot(model, type = "gap", xlim = c(-10, 10))
plot(model, type = "counterfactual", raw = "treated")

# take log_total as outcome
model_log <- gsynth(log_total ~ treat_post, 
               data = dt_analyze, index = c("census_t_1","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,5), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 7)
plot(model, type = "gap", xlim = c(-10, 10))
plot(model, type = "counterfactual", xlim = c(-10, 10))

# use delta
dt_analyze_delta = dt_analyze[!is.na(delta)]
model_delta <- gsynth(delta ~ treat_post, 
               data = dt_analyze_delta, index = c("census_t_1","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,5), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 7)
plot(model_delta, type = "gap", xlim = c(-10, 10))


# use quarterly data
dt_analyze_q = fread("dt_ready/crime_rate_q.csv")

model_q = gsynth(total ~ treat_post, 
               data = dt_analyze_q, index = c("census_t_1","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,5), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 7)
plot(model_q, type = "gap", xlim = c(-7, 10))

dt_analyze_q_delta = dt_analyze_q[!is.na(delta)]
model_q_delta = gsynth(delta ~ treat_post, 
               data = dt_analyze_q_delta, index = c("census_t_1","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,5), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 7)
plot(model_q_delta, type = "gap", xlim = c(-7, 10))

# take trimmed data
dt_analyze_q_trimmed = fread("dt_ready/crime_rate_q_trimmed.csv")
model_q_trimmed = gsynth(total ~ treat_post, 
               data = dt_analyze_q_trimmed, index = c("census_t_1","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,5), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 7)
plot(model_q_trimmed, type = "gap", xlim = c(-7, 10))
