setwd("~/Dropbox/masters thesis/Statcast linear weights/")

library(baseballr)

startYear <- 2015
endYear <- 2017

alldata <- c()  #clear alldata whenever this script is run
tmpdata <- c()

flush <- 50  #how many days to collect in temporary data frame before merging with alldata and flushing
count <- 0

# loop through years, then all dates in the baseball season
for (year in startYear:endYear) {
  # hard code the starting dates if you know them
  if (year==2015) {date <- as.Date("2015-04-05")}
  else if (year==2016) {date <- as.Date("2016-04-03")}
  else if (year==2017) {date <- as.Date("2017-04-02")}
  else {date <- as.Date(paste0(year,"-03-01"))}
  endDate <- as.Date(paste0(year,"-11-10"))
  while (date < endDate) {
    count <- count+1
    # attempt to scrape statcast data for each date
    tryCatch({oneday <- scrape_statcast_savant_batter_all(start_date=as.character(date), end_date=as.character(date))
              oneday[] <- lapply(oneday,factor)  # make all the columns factors so the dataframes can merge
              tmpdata <- rbind(tmpdata,oneday)   # merge into tmpdata
              print(paste0("Completed ",date))
              },
              warning=function(war) {print(paste0("---- Warning: ",date," ----"))},
              error=function(err) {print(paste0("**** Error: ",date," ****"))}
             )
    date <- date+1
    # periodically merge with alldata and flush tmpdata so we're not merging the huge data frame each time
    if (count==flush) {
      count <- 0
      alldata <- rbind(alldata,tmpdata)
      tmpdata <- c()
    }
  }
}
alldata <- rbind(alldata,tmpdata)  #merge at the end in case we haven't yet
print("Done downloading data.")
alldata$game_month <- format(as.Date(alldata$game_date,format="%Y-%m-%d"),"%m")

# add bbref IDs for batters
# crosswalk file is from https://github.com/chadwickbureau/register (as of 3/23/2018)
crosswalk <- read.csv("~/Documents/Projects/misc sports stuff/mlb_player_id_crosswalk.csv")
alldata <- merge(alldata, data.frame(batter=crosswalk$key_mlbam,bbref_id=crosswalk$key_bbref), by="batter", all.x=TRUE)

# add Lahman database IDs for batters
library(Lahman)
data("Batting")

# check if 2017 is in the Lahman R package yet
if (endYear==2017 & max(Batting$yearID)!=2017) {
  load("./lahman.master.2017.RData")
  df <- lahman.master.2017
} else {
  data("Master")
  df <- Master
}
alldata <- merge(alldata, data.frame(bbref_id=df$bbrefID,lahman_id=df$playerID), by="bbref_id", all.x=TRUE)

# get subset of batted balls
batted <- subset(alldata,description %in% c("hit_into_play","hit_into_play_no_out","hit_into_play_score","pitchout_hit_into_play_score"))
batted <- unique(batted)


# write data
print("Writing all pitches file.")
write.csv(alldata,"./all_pitches_by_batter_2015-2017.csv")
print("Writing batted balls file.")
write.csv(batted,"./batted_balls_2015-2017.csv")





