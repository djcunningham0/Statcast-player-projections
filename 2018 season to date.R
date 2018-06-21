source("./define_functions.R")

# note: should update speed scores CSV file whenever you run this during the season
# might also need to update mlb player ID crosswalk file

load("./data/batted_balls_2018-2018.RData")

tmp <- set_linear_weights(2018)
lw <- tmp$lw
lw_multiplier <- tmp$multiplier
batted.2018 <- format_data_frame(original_batted,lw)

library(randomForest)
probs.rf.speed <- predict(rf.speed, newdata=batted.2018, type="prob")
batted.2018 <- add_preds_from_probs("rf.speed", batted.2018, probs.rf.speed, lw)

lw.prefixes <- get_prefixes(batted.2018, type="lw")
full.prefixes <- get_prefixes(batted.2018, type="full")

library(data.table)
weights.dt.2018 <- group_weights_by_year(batted.2018)

load("./data/all_pitches_by_batter_2018-2018.RData")
batting.dt.2018 <- season_stats_from_statcast(alldata)

batting.dt.2018 <- merge(x=batting.dt.2018, y=weights.dt.2018, by.x=c("batter","yearID"), 
                         by.y=c("mlb_id","game_year"),all=TRUE)

colnames(batting.dt.2018)[colnames(batting.dt.2018)=="rf.speed_single"] <- "xX1B"
colnames(batting.dt.2018)[colnames(batting.dt.2018)=="rf.speed_double"] <- "xX2B"
colnames(batting.dt.2018)[colnames(batting.dt.2018)=="rf.speed_triple"] <- "xX3B"
colnames(batting.dt.2018)[colnames(batting.dt.2018)=="rf.speed_home_run"] <- "xHR"

batting.dt.2018$xH   <- with(batting.dt.2018, xX1B+xX2B+xX3B+xHR)
batting.dt.2018$xBA  <- with(batting.dt.2018, xH/AB)
batting.dt.2018$xOBP <- with(batting.dt.2018, (xH+BB+HBP)/PA)
batting.dt.2018$xSLG <- with(batting.dt.2018, (xX1B + 2*xX2B + 3*xX3B + 4*xHR)/AB)
batting.dt.2018$xOPS <- with(batting.dt.2018, xOBP+xSLG)
batting.dt.2018$xwOBA <- with(batting.dt.2018, (lw_multiplier * (lw["walk"]*(BB) + lw["HBP"]*HBP + 
                                                        lw["single"]*xX1B + lw["double"]*xX2B + 
                                                        lw["triple"]*xX3B + lw["home_run"]*xHR) / PA))

batting.dt.2018$wOBA_diff <- with(batting.dt.2018, wOBA-xwOBA)



