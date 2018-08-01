source("./define_functions.R")

original_batted.2018 <- readRDS("./data/current_season_statcast_batted_balls.rds")

tmp <- set_linear_weights(years=2018)
lw <- tmp$lw
lw_multiplier <- tmp$multiplier
batted.2018 <- format_data_frame(original_batted.2018, lw=lw, lw_multiplier=lw_multiplier)

library(randomForest)
rf <- readRDS("./models/rf.rds")
probs.rf.2018 <- predict(rf, newdata=batted.2018, type="prob")
batted.2018 <- add_preds_from_probs(batted.2018, "rf", probs.rf.2018, lw_year=2018)

weights.df.2018 <- group_weights_by_year(batted.2018)

batting.df.2018 <- readRDS("./data/current_season_summary.rds") %>% 
  full_join(weights.df.2018, by=c("key_mlbam", "Season")) %>% 
  rename(xX1B = rf_single,
         xX2B = rf_double,
         xX3B = rf_triple,
         xHR  = rf_home_run) %>% 
  mutate(xH    = xX1B+xX2B+xX3B+xHR,
         xBA   = xH / AB,
         xOBP  = (xH+BB+HBP) / PA,
         xSLG  = (xX1B + 2*xX2B + 3*xX3B + 4*xHR) / AB,
         xOPS  = xOBP + xSLG,
         xwOBA = (lw_multiplier * (lw["walk"]*(BB) + lw["HBP"]*HBP + 
                                     lw["single"]*xX1B + lw["double"]*xX2B + 
                                     lw["triple"]*xX3B + lw["home_run"]*xHR) / PA),
         wOBA_diff = round(wOBA - xwOBA, 4),
         HR_diff   = HR - xHR)

