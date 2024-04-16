# This builds analysis for the synthetic difference-in-differences estimator
# using the gsynth package.
rm(list=ls())
pacman::p_load(gsynth, data.table, panelView)
dt = fread("crime_agg_units.csv")
dt_e = fread("crime_agg_units_e.csv")
dt_analyze = dt[, c("tractce10", "month_year", "total", "econ_crime", "violent_crime", "drug_crime", "no_units", "stock_units")]
dt_analyze[, year := year(month_year)]
dt_analyze[, month := month(month_year)]
# Create the treatment variable: treated if stock_units > 0
dt_analyze$treat_post = fifelse(dt_analyze$stock_units > 0, 1, 0)

dt_analyze_e = dt_e[, c("tractce10", "month_year", "total", "econ_crime", "violent_crime", "drug_crime", "no_units", "stock_units")]
dt_analyze_e[, year := year(month_year)]
dt_analyze_e[, month := month(month_year)]
# Create the treatment variable: treated if stock_units > 0
dt_analyze_e$treat_post = ifelse(dt_analyze_e$stock_units > 0, 1, 0)

# Create the overall treatment variable - 1 if tract has ever been treated
# if ever treat_post = 1 then all values in treat = 1
dt_analyze$treat <- ifelse(dt_analyze$tractce10 %in% dt_analyze[treat_post == 1, tractce10], 1, 0)
dt_analyze_e$treat <- ifelse(dt_analyze_e$tractce10 %in% dt_analyze_e[treat_post == 1, tractce10], 1, 0)

View(dt_analyze[treat == 1])

# type of variables
str(dt_analyze)
# turn tractce10 into string
dt_analyze[, tractce10 := as.character(tractce10)]
dt_analyze_e[, tractce10 := as.character(tractce10)]
# turn total, econ_crime, violent_crime, drug_crime into numeric
dt_analyze[, total := as.numeric(total)]
dt_analyze[, econ_crime := as.numeric(econ_crime)]
dt_analyze[, violent_crime := as.numeric(violent_crime)]
dt_analyze[, drug_crime := as.numeric(drug_crime)]

dt_analyze_e[, total := as.numeric(total)]
dt_analyze_e[, econ_crime := as.numeric(econ_crime)]
dt_analyze_e[, violent_crime := as.numeric(violent_crime)]
dt_analyze_e[, drug_crime := as.numeric(drug_crime)]
# turn month_year into a time index, 1 for 01-01-1999, 2 for 01-02-1999, etc.
# extract month_year
month_year = unique(dt_analyze$month_year)
month_year_e = unique(dt_analyze_e$month_year)
# create a new column which is a row index in month_year
dt_analyze[, time_index := match(month_year, dt_analyze$month_year)]
dt_analyze_e[, time_index := match(month_year, dt_analyze_e$month_year)]

# When does usually the treatment start?
# For each tract, find the first time when stock_units > 0
dt_analyze[, first_treat := min(time_index[treat_post == 1]), by = tractce10]


panelview(total ~ treat_post, data = dt_analyze, 
          index = c("tractce10","time_index"), pre.post = TRUE, 
          by.timing = TRUE)

panelview(total ~ treat_post, data = dt_analyze, 
          index = c("tractce10","time_index"), type = "outcome", 
          main = "Public housing demolitions and total crimes", 
          by.group = TRUE)

model <- gsynth(total ~ treat_post, 
               data = dt_analyze, index = c("tractce10","time_index"), 
               se = TRUE, inference = "parametric", 
               r = 0, CV = FALSE, force = "two-way", 
               nboots = 1000, seed = 02139)
plot(model, type = "gap", xlim = c(-24, 24))
# Plot treated vs control lines
plot(model, type = "counterfactual", raw = "all")

# Change the treatment time by -2
# treat_post 2 = 1 when time_index >= first_treat - 2
dt_analyze$treat_post2 = fifelse(dt_analyze$time_index >= dt_analyze$first_treat - 2, 1, 0)
model_eviction <- gsynth(total ~ treat_post2, 
               data = dt_analyze, index = c("tractce10","time_index"), 
               se = TRUE, inference = "parametric", 
               r = 0, CV = FALSE, force = "two-way", 
               nboots = 1000, seed = 02139)
plot(model_eviction, type = "gap", xlim = c(-24, 24))

# Drop treated tract from first 2 years
dt_analyze_rc = dt_analyze[!(tractce10 %in% dt_analyze[treat_post == 1 & time_index <= 24, tractce10])]
# count treated tracts
length(unique(dt_analyze_rc[tractce10 %in% dt_analyze_rc[treat_post == 1, tractce10], tractce10]))

model_2y <- gsynth(total ~ treat_post, 
               data = dt_analyze_rc, index = c("tractce10","time_index"), 
               se = TRUE, inference = "parametric", 
               r = 0, CV = FALSE, force = "two-way", 
               nboots = 1000, seed = 02139)
plot(model_2y, type = "gap", xlim = c(-24, 24))

### TEST
data(gsynth)
names(turnout)
head(turnout)
str(turnout)
as.data.table(turnout)
panelview(turnout ~ policy_edr, data = turnout, 
          index = c("abb","year"), pre.post = TRUE, 
          by.timing = TRUE)

out0 <- gsynth(turnout ~ policy_edr , 
               data = turnout, index = c("abb","year"), 
               se = TRUE, inference = "parametric", 
               r = 0, CV = FALSE, force = "two-way", 
               nboots = 1000, seed = 02139)
