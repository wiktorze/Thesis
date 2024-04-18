# This builds analysis for the synthetic difference-in-differences estimator
# using the gsynth package.
rm(list=ls())
pacman::p_load(gsynth, data.table, panelView, ggplot2)
dt = fread("crime_rate.csv")
View(dt)
summary(dt$crime_rate)
summary(dt$pop)
summary(dt$total)

dt_analyze = dt[, c("census_t_1", "month_year", "total", "crime_rate", "econ_crime", "violent_crime", "drug_crime", "no_units", "stock_units", "pop")]
dt_analyze[, year := year(month_year)]
dt_analyze[, month := month(month_year)]
# Create the treatment variable: treated if stock_units > 0
dt_analyze$treat_post = fifelse(dt_analyze$stock_units > 0, 1, 0)

# Create the overall treatment variable - 1 if tract has ever been treated
# if ever treat_post = 1 then all values in treat = 1
dt_analyze$treat <- ifelse(dt_analyze$census_t_1 %in% dt_analyze[treat_post == 1, census_t_1], 1, 0)

View(dt_analyze[treat == 1])
# count number of treated tracts in general
dt_analyze[treat == 1, .N, by = census_t_1]
# 39 treated tracts
sum(dt_analyze$no_units)
# type of variables
str(dt_analyze)
# turn census_t_1 into string
dt_analyze[, census_t_1 := as.character(census_t_1)]
# turn total, econ_crime, violent_crime, drug_crime into numeric
dt_analyze[, crime_rate := as.numeric(crime_rate)]
dt_analyze[, total := as.numeric(total)]
dt_analyze[, econ_crime := as.numeric(econ_crime)]
dt_analyze[, violent_crime := as.numeric(violent_crime)]
dt_analyze[, drug_crime := as.numeric(drug_crime)]

# turn month_year into a time index, 1 for 01-01-1999, 2 for 01-02-1999, etc.
# extract month_year
month_year = unique(dt_analyze$month_year)
# create a new column which is a row index in month_year
dt_analyze[, time_index := match(month_year, dt_analyze$month_year)]

# When does usually the treatment start?
# For each tract, find the first time when stock_units > 0
dt_analyze[, first_treat := min(time_index[treat_post == 1]), by = census_t_1]

# get rid of ticks on x-axis
png("Figures/treatment_time.png")
panelview(total ~ treat_post, data = dt_analyze, 
          index = c("census_t_1","time_index"), pre.post = TRUE, 
          by.timing = TRUE, xlab = "Month", ylab = "Tracts", cex.axis.x = 2)
dev.off()

panelview(total ~ treat_post, data = dt_analyze, 
          index = c("census_t_1","time_index"), type = "outcome", 
          main = "Public housing demolitions and total crimes", 
          by.group = TRUE)

model <- gsynth(total ~ treat_post + stock_units, 
               data = dt_analyze, index = c("census_t_1","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,5), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139)
png("Figures/synth_did_total.png")
plot(model, type = "gap")
dev.off()
# Plot treated vs control lines
plot(model, type = "counterfactual", raw = "treated")

# Change the treatment time by -2
# treat_post 2 = 1 when time_index >= first_treat - 2
dt_analyze$treat_post2 = fifelse(dt_analyze$time_index >= dt_analyze$first_treat - 2, 1, 0)
model_eviction <- gsynth(total ~ treat_post2, 
               data = dt_analyze, index = c("census_t_1","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,5), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139)
plot(model_eviction, type = "gap")

# Drop treated tract from first 2 years
model_2y <- gsynth(total ~ treat_post, 
               data = dt_analyze, index = c("census_t_1","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,5), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 24)
plot(model_2y, type = "gap")
plot(model_2y, type = "counterfactual", raw = "all")


# use crime_rate instead of total
model_crime_rate <- gsynth(crime_rate ~ treat_post, 
               data = dt_analyze, index = c("census_t_1","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,5), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139)

png("Figures/synth_did_crime_rate.png")
plot(model_crime_rate, type = "gap")
dev.off()

# Use quarterly data
dt_analyze_q = fread("crime_rate_q_trimmed_adj.csv")
dt_analyze_q[, q_year := paste(quarter, year.x, sep = "/")]
quarter_year = unique(dt_analyze_q$q_year)
# create a new column which is a row index in q_year
dt_analyze_q[, time_index := match(q_year, dt_analyze_q$q_year)]
# Treated variable
dt_analyze_q$treat_post = fifelse(dt_analyze_q$stock_units > 0, 1, 0)
dt_analyze_q$treat <- ifelse(dt_analyze_q$census_t_1 %in% dt_analyze_q[treat_post == 1, census_t_1], 1, 0)


model_crime_rate_q <- gsynth(crime_rate ~ treat_post, 
               data = dt_analyze_q, index = c("census_t_1","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,5), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 7)
png("Figures/synth_did_crime_rate_q.png")
plot(model_crime_rate_q, type = "gap", xlim = c(-7, 20))
dev.off()
model_crime_rate_q$Ntr
# see how many units demolished in total 
sum(dt_analyze_q$no_units)

### increase min.T0 to 14
model_crime_rate_q_late <- gsynth(crime_rate ~ treat_post, 
               data = dt_analyze_q, index = c("census_t_1","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,5), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 14)
png("Figures/synth_did_crime_rate_q_late.png")
plot(model_crime_rate_q_late, type = "gap", xlim = c(-14, 20))
dev.off()
model_crime_rate_q_late$Ntr

### Use adjusted population
model_crime_rate_q_adj <- gsynth(crime_rate_adj ~ treat_post, 
               data = dt_analyze_q, index = c("census_t_1","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,5), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 7)
png("Figures/synth_did_crime_rate_q_adj.png")
plot(model_crime_rate_q_adj, type = "gap", xlim = c(-7, 20))
dev.off()
model_crime_rate_q_adj$Ntr

### Use adjusted population and min.T0 = 14
model_crime_rate_q_adj_late <- gsynth(crime_rate_adj ~ treat_post, 
               data = dt_analyze_q, index = c("census_t_1","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,5), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 14)
png("Figures/synth_did_crime_rate_q_adj_late.png")
plot(model_crime_rate_q_adj_late, type = "gap", xlim = c(-14, 20))
dev.off()

# take log of crime_rate
# remove all tracts where crime_rate = 0
tract_to_remove = unique(dt_analyze_q[crime_rate == 0, census_t_1])
dt_analyze_q_log = dt_analyze_q[!census_t_1 %in% tract_to_remove]
dt_analyze_q_log[, crime_rate_log := log(crime_rate)]
model_crime_rate_q_log <- gsynth(crime_rate_log ~ treat_post, 
               data = dt_analyze_q_log, index = c("census_t_1","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,5), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 7)
png("Figures/synth_did_crime_rate_q_log.png")
plot(model_crime_rate_q_log, type = "gap", xlim = c(-7, 30))
dev.off()
model_crime_rate_q_log$Ntr

# population adjusted
dt_analyze_q_log[, crime_rate_log_adj := log(crime_rate_adj)]
model_crime_rate_q_log_adj <- gsynth(crime_rate_log_adj ~ treat_post, 
               data = dt_analyze_q_log, index = c("census_t_1","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,5), CV = TRUE, force = "two-way", 
               nboots = 1000, seed = 02139, min.T0 = 7)

plot(model_crime_rate_q_log_adj, type = "gap", xlim = c(-7, 30))
model_crime_rate_q_log_adj$Ntr
