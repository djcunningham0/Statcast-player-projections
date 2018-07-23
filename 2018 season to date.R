source("./define_functions.R")

original_batted.2018 <- readRDS("./data/current_season_statcast_batted_balls.rds")

tmp <- set_linear_weights(2018)
lw <- tmp$lw
lw_multiplier <- tmp$multiplier
batted.2018 <- format_data_frame(original_batted.2018, lw)

library(randomForest)
rf <- readRDS("./models/rf.rds")
probs.rf.2018 <- predict(rf, newdata=batted.2018, type="prob")
batted.2018 <- add_preds_from_probs(batted.2018, "rf", probs.rf.2018, lw)

probs.rf.update.2018 <- predict(rf.update, newdata=batted.2018, type="prob")
batted.2018 <- add_preds_from_probs(batted.2018, "rf.update", probs.rf.update.2018, lw)

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
         HR_diff   = HR - xHR) %>% 
  rename(xX1B.2 = rf.update_single,
         xX2B.2 = rf.update_double,
         xX3B.2 = rf.update_triple,
         xHR.2  = rf.update_home_run) %>% 
  mutate(xH.2    = xX1B.2+xX2B.2+xX3B.2+xHR.2,
         xBA.2   = xH.2 / AB,
         xOBP.2  = (xH.2+BB+HBP) / PA,
         xSLG.2  = (xX1B.2 + 2*xX2B.2 + 3*xX3B.2 + 4*xHR.2) / AB,
         xOPS.2  = xOBP.2 + xSLG.2,
         xwOBA.2 = (lw_multiplier * (lw["walk"]*(BB) + lw["HBP"]*HBP + 
                                     lw["single"]*xX1B.2 + lw["double"]*xX2B.2 + 
                                     lw["triple"]*xX3B.2 + lw["home_run"]*xHR.2) / PA),
         wOBA_diff.2 = round(wOBA - xwOBA.2, 4),
         HR_diff.2   = HR - xHR.2)

View(batting.df.2018 %>% 
       filter(AB >= 100) %>% 
       select(Name, Team, position, AB, wOBA, xwOBA, xwOBA.2, wOBA_diff, wOBA_diff.2,
              HR, xHR, xHR.2, HR_diff, HR_diff.2), '.')
