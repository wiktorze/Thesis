pacman::p_load(scpi, data.table)
rm(list=ls())
dt_analyze_q = read.csv("dt_analyze_q.csv")
dt_analyze_q = dt_analyze_q[dt_analyze_q$census_t_1 != "17031283800",]

outcome.var <- "crime_rate_adj"
id.var <- "census_t_1"
treatment.var <- "treat_post"
time.var <- "time_index"
df.unit <- scdataMulti(dt_analyze_q, id.var = id.var, outcome.var = outcome.var,
                        treatment.var = treatment.var,
                        time.var = time.var,
                    cointegrated.data = TRUE, constant = TRUE)
summary(df.unit)
res.unit <- scpi(df.unit, sims = 10, cores = 8)
scplotMulti(res.unit, joint = TRUE)
