pacman::p_load(data.table)
dt = fread("dt_analyze.csv")
View(dt[treat_post == 1])
# Display first_treat for each census_t_1
dt[treat_post == 1, .(first_treat = first(first_treat)), by = census_t_1]
