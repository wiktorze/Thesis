pacman::p_load(data.table, ggplot2)
dt = fread("dt_analyze.csv")
View(dt[treat_post == 1])
# Display first_treat for each census_t_1, order by first_treat
dt[treat_post == 1, .(first_treat = first(first_treat)), by = census_t_1][order(first_treat)]

# plot stock units evolution over time in treated tracts
# add a line at stock_units = 75
dt[treat == 1, .(census_t_1, time_index, stock_units)][, ggplot(.SD, aes(x = time_index, y = stock_units, group = census_t_1)) + geom_line(lwd = 1.5) + labs(title = "Stock units evolution over time in treated tracts") + geom_hline(yintercept = 75, linetype = "dashed", lwd = 2, color = "red") + scale_y_continuous(breaks=seq(0,2000,75)) + theme_minimal()]
