# This builds analysis for the synthetic difference-in-differences estimator
# using the gsynth package.
pacman::p_load(gsynth, data.table)
dt = fread("crime_agg_units.csv")
dt_analyze = dt[, c("tractce10", "month_year", "total", "econ_crime", "violent_crime", "drug_crime", "no_units", "stock_units")]

result <- gsynth(
     data = dt_analyze,
     yname = "total",  
     idname = "tractce10",         
     time = "month_year",               
     treatmentname = "no_units", 
     force = "pre",               
     method = "iridge"            
)
