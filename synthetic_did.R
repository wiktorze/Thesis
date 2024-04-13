# This builds analysis for the synthetic difference-in-differences estimator
# using the gsynth package.
rm(list=ls())
pacman::p_load(gsynth, data.table, panelView)
dt = fread("crime_agg_units.csv")

dt_analyze = dt[, c("tractce10", "month_year", "total", "econ_crime", "violent_crime", "drug_crime", "no_units", "stock_units")]
dt_analyze[, year := year(month_year)]
dt_analyze[, month := month(month_year)]
# Create the treatment variable: treated if stock_units > 0
dt_analyze$treat_post = ifelse(dt_analyze$stock_units > 0, 1, 0)

# Create the overall treatment variable - 1 if tract has ever been treated
# if ever treat_post = 1 then all values in treat = 1
dt_analyze$treat <- ifelse(dt_analyze$tractce10 %in% dt_analyze[treat_post == 1, tractce10], 1, 0)
View(dt_analyze[treat == 1])

# type of variables
str(dt_analyze)
# turn tractce10 into string
dt_analyze[, tractce10 := as.character(tractce10)]
# turn total, econ_crime, violent_crime, drug_crime into numeric
dt_analyze[, total := as.numeric(total)]
dt_analyze[, econ_crime := as.numeric(econ_crime)]
dt_analyze[, violent_crime := as.numeric(violent_crime)]
dt_analyze[, drug_crime := as.numeric(drug_crime)]
# turn month_year into a time index, 1 for 01-01-1999, 2 for 01-02-1999, etc.
# extract month_year
month_year = unique(dt_analyze$month_year)
# create a new column which is a row index in month_year
dt_analyze[, time_index := match(month_year, dt_analyze$month_year)]


panelview(total ~ treat_post, data = dt_analyze, 
          index = c("tractce10","time_index"), pre.post = TRUE, 
          by.timing = TRUE)


out0 <- gsynth(total ~ treat_post, 
               data = dt_analyze, index = c("tractce10","time_index"), 
               se = TRUE, inference = "parametric", 
               r = c(0,5), CV = FALSE, force = "two-way", 
               nboots = 1000, seed = 02139)
plot(out0, type = "gap")

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
