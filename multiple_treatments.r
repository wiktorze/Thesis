# This file modifies dt to analyze into multiple treatments for the same tract being separated with a unique id
pacman::p_load(data.table)
dt = fread("dt_analyze.csv")

# everytime no_units > 50 for the same census_t_1, the treatment changes
# so treat_change = 0 until no_units > 50, then treat_change = 1 for next periods, then treat_change = 2 if no_units >50 again, etc.
dt[, treat_change := cumsum(no_units > 50), by = census_t_1]
# create id column that combines census_t_1 and treat_change
dt[, id := paste0(census_t_1, "_", treat_change)]

dt[, diff_treat := treat_change - shift(treat_change, fill = 0)]  # Calculate the difference

# Function to save the data table
save_filtered_data <- function(data, census_t_1, treat_change) {
  filename <- paste0("census_t_", census_t_1, "_", treat_change, ".csv")
  fwrite(data, filename)
}

# Apply the filtering and saving 
dt[diff_treat == 1, {
   filtered_data := .SD[time_index >= (time_index - 12) & 
                        time_index <= (time_index + 12)]
   save_filtered_data(filtered_data, census_t_1, treat_change)  # Save!
}, by = census_t_1] 

