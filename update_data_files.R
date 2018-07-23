#' wrapper function that calls the other functions defined below
update_data <- function(season.year=get_current_season_year(),
                        last.season.year=get_last_completed_season_year(),
                        path="/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/data/") {
  require(readr, quietly=TRUE, warn.conflicts=FALSE)
  
  path <- format_directory_path(path)
  update.hist <- read_csv(paste0(path, "last_updates.csv"), col_types=cols())
  
  cur.date  <- Sys.Date()
  cur.month <- as.numeric(format(cur.date, "%m"))
  
  ### begin daily runs ###
  update_linear_weights(path=path)
  update_crosswalk(path=path)
  update_speed_scores(path=path, end_year=season.year)
  ### end daily runs ###
  
  ### begin less frequent runs ###
  # current season summary:
  #   - daily between April and October because that's when it should change
  #   - otherwise every 30 days in case there are corrections
  if ((cur.month >= 4 & cur.month <= 10) | (days_since(update.hist$current_season_summary) >= 30)) {
    update_current_season_summary(year=season.year, path=path)
  }
  
  # completed seasons summary:
  #   - every 30 days since it shouldn't change
  if (days_since(update.hist$completed_seasons_summary) >= 30) {
    update_completed_seasons_summary(start_year=2013, end_year=last.season.year, path=path)
  }
  
  # current season statcast:
  #   - append every day April through October
  #   - full rebuild every 30 days in case there are corrections
  if (days_since(update.hist$current_season_full_rebuild) >= 30) {
    update_current_season_statcast(year=season.year, path=path, full_rebuild=TRUE)
  } else if (cur.month > 4 & cur.month < 11) {
    update_current_season_statcast(year=season.year, path=path, full_rebuild=FALSE)
  }
  
  # completed season statcast:
  #   - full rebuild once after a season ends (11/10)
  #   - full rebuild once just before a new season begins (3/20)
  season.end.check <- as.Date(paste0(last.season.year,"-11-10"))
  season.start.check <- as.Date(paste0(last.season.year + 1, "-03-20"))
  if (cur.date >= season.end.check & update.hist$completed_seasons_statcast < season.end.check) {
    rebuild_completed_seasons_statcast(start_year=2015, end_year=last.season.year, path=path)
  } else if (cur.date >= season.start.check & update.hist$completed_seasons_statcast < season.start.check) {
    rebuild_completed_seasons_statcast(start_year=2015, end_year=last.season.year, path=path)
  }
  ### end less frequent runs ###
}

#' Return the number of days since a given date.
days_since <- function(date) {
  return(as.numeric(Sys.Date() - as.Date(date)))
}

#' don't necessarily need to save this locally since fg_guts() is easy, but this allows
#' me to get linear weights w/o an internet connection
update_linear_weights <- function(path="/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/data/") {
  require(baseballr, quietly=TRUE, warn.conflicts=FALSE)
  
  path <- format_directory_path(path)
  
  saveRDS(fg_guts(), file=paste0(path, "linear_weight_coefs.rds"))
  
  # stamp today's date in the last update file
  register_updates("linear_weights", path=path)
}

#' update mlb_player_id_crosswalk.rds
update_crosswalk <- function(path="/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/data/") {
  require(readr, quietly=TRUE, warn.conflicts=FALSE)
  require(dplyr, quietly=TRUE, warn.conflicts=FALSE)
  
  path <- format_directory_path(path)
  
  url <- "http://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv"
  crosswalk <- read_csv(url, col_types=cols()) %>% 
    # only keep if they have at least one of the IDs I'll be using
    filter(!(is.na(key_mlbam) & is.na(key_bbref) & is.na(key_fangraphs)))
  
  saveRDS(crosswalk, file=paste0(path, "mlb_player_id_crosswalk.rds"))
  
  # stamp today's date in the last update file
  register_updates("crosswalk", path=path)
}

#' update the speed_scores.rds
update_speed_scores <- function(start_year=2015, end_year=get_current_season_year(), agg=FALSE,
                                path="/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/data/") {
  path <- format_directory_path(path)
  
  # scrape speed scores and update the RDS file
  speed_scores <- scrape_fangraphs(start_year=start_year, end_year=end_year, agg=agg, which="Spd")
  saveRDS(speed_scores, file=paste0(path, "speed_scores.rds"))
  
  # stamp today's date in the last update file
  register_updates("speed_scores", path=path)
}

#' 
update_current_season_summary <- function(year=get_current_season_year(),
                                          path="/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/data/") {
  path <- format_directory_path(path) 
  
  # scrape stats for all years and update the RDS file
  speed_scores <- scrape_fangraphs(start_year=year, end_year=year, agg=FALSE, which="full")
  saveRDS(speed_scores, file=paste0(path, "current_season_summary.rds"))
  
  # stamp today's date in the last update file
  register_updates("current_season_summary", path=path)
}

#' 
update_completed_seasons_summary <- function(start_year=2013, end_year=get_last_completed_season_year(),
                                             path="/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/data/") {
  path <- format_directory_path(path)
  
  # scrape stats for all years and update the RDS file
  speed_scores <- scrape_fangraphs(start_year=start_year, end_year=end_year, agg=FALSE, which="full")
  saveRDS(speed_scores, file=paste0(path, "completed_seasons_summary.rds"))
  
  # stamp today's date in the last update file
  register_updates("completed_seasons_summary", path=path)
}

#'
update_current_season_statcast <- function(year=get_current_season_year(), 
                                           full_rebuild=FALSE,
                                           path="/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/data/",
                                           all.pitches.name="current_season_statcast_all_pitches.rds",
                                           batted.name="current_season_statcast_batted_balls.rds") {
  require(dplyr, quietly=TRUE, warn.conflicts=FALSE)
  
  path <- format_directory_path(path)
  
  if (!file.exists(paste0(path, batted.name))) {
    # always do a full rebuild if the file doesn't exist
    full_rebuild <- TRUE
  } else {
    cur.batted.file  <- readRDS(paste0(path, batted.name))
    cur.pitches.file <- readRDS(paste0(path, all.pitches.name))
    if (max(cur.batted.file$game_year) != year) {
      # always do a full rebuild if the existing file has last year's data
      full_rebuild <- TRUE
    } else {
      # get the last pulled date so we can build from the next day if doing partial build
      last.date <- max(cur.batted.file$game_date)
      next.date <- format(last.date+1, "%m-%d")
    }
  }
  
  if (full_rebuild) {
    # build the file from scratch
    print("Beginning full rebuild of current season Statcast files.")
    pull_statcast_data(startYear=year, endYear=year, directory=path,
                       pitches_file=all.pitches.name,
                       batted_file=batted.name)
    print("Completed full rebuild of current season Statcast files.")
    
    register_updates("current_season_statcast", path=path)
    register_updates("current_season_full_rebuild", path=path)
  } else {
    # partial build: add the days that haven't been pulled yet
    print("Beginning partial build of current season Statcast files.")
    out <- pull_statcast_data(startYear=year, endYear=year, directory=NULL,
                              startDate=next.date)
    
    # coerce everything to character, then merge, then reformat, then save
    cur.pitches.file <- cur.pitches.file %>% 
      mutate_all(as.character)
    cur.batted.file <- cur.batted.file %>% 
      mutate_all(as.character)
    
    all_pitches <- bind_rows(cur.pitches.file, out$all) %>% 
      format_statcast_fields()
    batted <- bind_rows(cur.batted.file, out$batted) %>% 
      format_statcast_fields()
    
    saveRDS(all_pitches, file=paste0(path, all.pitches.name))
    saveRDS(batted, file=paste0(path, batted.name))
    
    print("Completed partial build of current season Statcast files.")
    
    register_updates("current_season_statcast", path=path)
  }
}

#' 
rebuild_completed_seasons_statcast <- function(start_year=2015, 
                                               end_year=get_last_completed_season_year(),
                                               path="/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/data/",
                                               all.pitches.name="completed_seasons_statcast_all_pitches.rds",
                                               batted.name="completed_seasons_statcast_batted_balls.rds") {
  path <- format_directory_path(path)
  
  print("Beginning full rebuild of completed season Statcast files.")
  pull_statcast_data(startYear=start_year, endYear=end_year, directory=path,
                     pitches_file=all.pitches.name,
                     batted_file=batted.name)
  print("Completed full rebuild of completed season Statcast files.")
  
  register_updates("completed_seasons_statcast", path=path)
}

#' Get the year of the current MLB season
#' 
#' if 4/8 or later, assume regular season is in progress and use this year;
#' otherwise, use last year
#' this is pretty conservative -- want to avoid getting errors trying to pull Statcast data before season starts
#' 
#' @param season_start season start date in "mm-dd" format (default is 4/8)
get_current_season_year <- function(season_start="04-08") {
  cur.date  <- Sys.Date()
  cur.year  <- as.numeric(format(cur.date, "%Y"))
  
  year <- ifelse(cur.date >= as.Date(paste0(cur.year,"-",season_start)), cur.year, cur.year-1)
  
  return(year)
}

#' Get the year of the last completed MLB season
#' 
#' if 11/10 or later, assume regular season is in progress and use this year;
#' otherwise, use last year
#' this is really conservative -- want to avoid saying a season is complete if it isn't
#' 
#' @param season_end season end date in "mm-dd" format (default is 11/10)
get_last_completed_season_year <- function(season_end="11-10") {
  cur.date  <- Sys.Date()
  cur.year  <- as.numeric(format(cur.date, "%Y"))
  
  year <- ifelse(cur.date >= as.Date(paste0(cur.year,"-",season_end)), cur.year, cur.year-1)
  
  return(year)
}

#' basically a copy of baseballr::fg_bat_leaders but adds fangraphs ID only includes Spd
scrape_fangraphs <- function(start_year=2018, end_year=start_year, agg=FALSE, which="full",
                             path="/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/data/") {
  require(xml2, quietly=TRUE, warn.conflicts=FALSE)
  require(rvest, quietly=TRUE, warn.conflicts=FALSE)
  require(dplyr, quietly=TRUE, warn.conflicts=FALSE)
  require(stringr, quietly=TRUE, warn.conflicts=FALSE)
  
  path <- format_directory_path(path)
  
  if (which == "Spd") {
    type_list <- "2,60"  # 2=year, 60=Spd
  } else if (which == "full") {
    # year, age, G, AB, PA, H, 1B, 2B, 3B, HR, R, RBI, BB, IBB, SO, HBP, SF, SH, SB, CS, AVG, OBP, SLG, OPS, wOBA
    type_list <- "2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,21,22,23,37,38,39,50"
  }
  type_list <- paste0("c,", type_list)
  
  # generate url for fangraphs leaderboard for the specified years
  url <- paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0",
                "&type=", type_list, "&season=", end_year, "&month=0&season1=", start_year, "&ind=", 
                as.numeric(!agg), "&team=&rost=&age=&filter=&players=&page=1_100000")
  
  # read the html from the url
  out <- read_html(url) %>% html_nodes("table") %>% `[[`(12)
  
  # extract the hyperlinks that contain the player's fangraphs ID and position
  links <- html_children(out)[4]
  links <- html_nodes(links, "a")
  links <- html_attr(links, "href")
  links <- str_subset(links, "playerid=")
  
  # links are "statss.aspx?playerid=<PLAYERID>&position=<POS>"
  player_ids <- sapply(str_split(sapply(str_split(links, "playerid="), '[', 2), "&"), '[', 1)
  pos <- sapply(str_split(links, "position="), '[', 2)
  
  # now resume pulling the HTML table (same as baseballr::fg_bat_leaders)
  out <- out %>% html_table(fill=TRUE)
  out <- out[-c(1,3),]
  out <- out[,-1]
  names(out) <- out[1,]
  out <- out[-1,]
  if (agg == FALSE) {
    out <- out[,-1]  # season is automatically pulled in if not aggregating (so remove the duplicate column)
  }
  
  # format some column names (probably not necessary for the columns I'll be working with)
  c <- as.matrix(names(out))
  c <- gsub("%", "_pct", c, fixed = TRUE)
  c <- gsub(" (pfx)", "_pfx", c, fixed = TRUE)
  c <- gsub(" (pi)", "_pi", c, fixed = TRUE)
  c <- gsub("/", "_", c, fixed = TRUE)
  c <- ifelse(substr(c, nchar(c)-1+1, nchar(c)) == ".", gsub("\\.", "_pct", c), c)
  names(out) <- c
  
  out$key_fangraphs <- as.integer(player_ids)
  out$position <- pos
  
  # use crosswalk to add mlb and bbref IDs to scraped data
  file <- paste0(path, "mlb_player_id_crosswalk.rds")
  crosswalk <- readRDS(file) %>% 
    select(key_mlbam, key_bbref, key_fangraphs)
  out <- out %>% 
    left_join(crosswalk, by="key_fangraphs") %>% 
    select(Name, key_mlbam, key_bbref, key_fangraphs, position, Team, everything()) %>% 
    # format some values
    format_scraped_fg_data(which=which)
  
  return(out)
}

#' make sure scraped data has correct format (convert from character to integer/numeric)
format_scraped_fg_data <- function(df, which="full") {
  require(dplyr, quietly=TRUE, warn.conflicts=FALSE)
  
  if (which == "full") {
    df <- df %>% 
      rename("X1B" = `1B`,
             "X2B" = `2B`,
             "X3B" = `3B`)
    
    int.cols <- c("Season", "Age", "G", "AB", "PA", "H", "X1B", "X2B", "X3B", "HR",
                  "R", "RBI", "BB", "IBB", "SO", "HBP", "SF", "SH", "SB", "CS")
    num.cols <- c("AVG", "OBP", "SLG", "OPS", "wOBA")
  } else if (which == "Spd") {
    int.cols <- "Season"
    num.cols <- "Spd"
  }
  
  df <- df %>% 
    mutate_at(int.cols, as.integer) %>% 
    mutate_at(num.cols, as.numeric)
  
  return(df)
}

#' record today's date in last_updates.csv
register_updates <- function(which, path) {
  require(readr, quietly=TRUE, warn.conflicts=FALSE)
  
  path <- format_directory_path(path)
  
  update_df <- read_csv(paste0(path, "last_updates.csv"), col_types=cols())
  update_df[,which] <- Sys.Date()
  write_csv(update_df, paste0(path, "last_updates.csv"))
}

#' Reset last_updates.csv so that every category says it was last updated 1/1/1900
#' 
#' @param which if specified, only reset this file's date; otherwise reset everything
reset_last_updates_file <- function(which=NULL,
                                    path="/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/data/") {
  require(readr, quietly=TRUE, warn.conflicts=FALSE)
  require(dplyr, quietly=TRUE, warn.conflicts=FALSE)
  
  path <- format_directory_path(path)
  
  d <- as.Date("1900-01-01")
  if (!is.null(which)) {
    update_df <- read_csv(paste0(path, "last_updates.csv"), col_types=cols())
    update_df[,which] <- d
  } else {
    update_df <- tibble(linear_weights=d,
                        crosswalk=d,
                        speed_scores=d,
                        current_season_summary=d,
                        completed_seasons_summary=d,
                        current_season_statcast=d,
                        completed_seasons_statcast=d,
                        current_season_full_rebuild=d
    )
  }
  
  write_csv(update_df, paste0(path, "last_updates.csv"))
}

#' make sure directory path ends in "/"
format_directory_path <- function(path) {
  require(stringr, quietly=TRUE, warn.conflicts=FALSE)
  if (str_sub(path, -1) != "/") {
    path <- paste0(path, "/")
  }
  return(path)
}

#' Pull Statcast data for a given date range. Uses baseballr::scrape_statcast_savant_batter_all
#' 
#' @param startYear first year to pull Statcast data
#' @param endYear last year to pull Statcast data
#' @param directory directory to write .rds files to; set to NULL to return list of data frames instead
#' @param pitches_file file name for all pitches file
#' @param batted_file file name for batted balls file
#' @param flush how many days to collect in temporary data frame before merging with alldata and flushing
#' @param startDate start date in "mm-dd" format
#' @param endDate end date in "mm-dd" format
#' @param showTime if TRUE, print the total elapsed time
pull_statcast_data <- function(startYear, endYear=startYear, 
                               directory="/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/data/", 
                               pitches_file=str_c(directory, "all_pitches_by_batter_",startYear,"-",endYear,".rds"),
                               batted_file=str_c(directory, "batted_balls_",startYear,"-",endYear,".rds"),
                               flush=50, startDate=NA, endDate=NA, showTime=FALSE) {
  require(baseballr, quietly=TRUE, warn.conflicts=FALSE)
  require(readr, quietly=TRUE, warn.conflicts=FALSE)
  require(dplyr, quietly=TRUE, warn.conflicts=FALSE)
  require(stringr, quietly=TRUE, warn.conflicts=FALSE)
  require(lubridate, quietly=TRUE, warn.conflicts=FALSE)
  
  if (showTime) {
    require(tictoc, quietly=TRUE, warn.conflicts=FALSE)
    tic()
  }
  
  # print message if invalid directory was specified
  if (!is.null(directory) && (directory != "") && !file.exists(directory)) {
    print("Specified directory does not exist.")
    if (str_sub(directory, 1, 2) == "./") {
      directory <- str_replace(directory, "./", paste0(getwd(),"/"))
    }
    print(directory)
    return(NULL)
  }
  
  alldata <- c()
  tmpdata <- c()
  count <- 0
  
  # loop through years, then all dates in the baseball season
  for (year in startYear:endYear) {
    # use dates if specified
    if (!is.na(startDate)) {date <- ymd(str_c(year, "-", startDate))}
    # otherwise hard code the starting dates if you know them
    else if (year==2015) {date <- ymd("2015-04-05")}
    else if (year==2016) {date <- ymd("2016-04-03")}
    else if (year==2017) {date <- ymd("2017-04-02")}
    else if (year==2018) {date <- ymd("2018-03-29")}
    # otherwise use a date that's definitely before the regular season starts
    else {date <- as.Date(str_c(year,"-03-29"))}
    # same idea for end date
    if (!is.na(endDate)) {end <- ymd(str_c(year, "-", endDate))}
    else if (year==2015) {end <- ymd("2015-11-02")}
    else if (year==2016) {end <- ymd("2016-11-03")}
    else if (year==2017) {end <- ymd("2017-11-02")}
    else {end <- min(ymd(str_c(year,"-11-10")), Sys.Date())}
    while (date <= end) {
      count <- count + 1
      oneday <- NULL
      # attempt to scrape statcast data for each date
      # do it one day at a time because the function doesn't work well when pulling many days
      tryCatch(
        {oneday <- scrape_statcast_savant_batter_all(start_date=as.Date(date), 
                                                     end_date=as.Date(date)) 
        },
        warning=function(war) {print(str_c("---- Warning: ",date," ----"))},
        error=function(err) {print(str_c("**** Error: ",date," ****"))}
      )
      if (!is.null(oneday)) {
        # convert all to characters for now to avoid merging errors (reformat later)
        oneday <- oneday %>% 
          mutate_all(as.character)
        tmpdata <- bind_rows(tmpdata, oneday)   # merge into tmpdata
        print(str_c("Completed ",date))
      }
      date <- date+1
      # periodically merge with alldata and flush tmpdata so we're not merging the huge data frame each time
      if (count==flush) {
        count <- 0
        alldata <- bind_rows(alldata,tmpdata)
        tmpdata <- c()
      }
    }
  }
  if (!is.null(tmpdata)) {
    alldata <- bind_rows(alldata,tmpdata) %>%   #merge at the end in case we haven't yet
      format_statcast_fields() %>% 
      mutate(game_month = month(game_date))
  }
  print("Done downloading data.")
  
  if (is.null(alldata)) {
    print("No data for date range.")
    return(NULL)
  }
  
  # get subset of batted balls
  batted <- alldata %>% 
    filter(description %in% c("hit_into_play","hit_into_play_no_out","hit_into_play_score",
                              "pitchout_hit_into_play_score")) %>% 
    distinct()
  
  if (!is.null(directory) && (directory != "")) {
    # write data
    # add "/" to directory if it isn't there already
    directory <- format_directory_path(directory)
    
    print("Writing all pitches file...")
    saveRDS(alldata, file=paste0(directory, pitches_file))
    
    print("Writing batted balls file...")
    saveRDS(batted, file=paste0(directory, batted_file))
    
    if (showTime) { toc() }
  }
  else { 
    if (showTime) { toc() }
    return(list(all=alldata, batted=batted)) 
  }
}

#' format each field correctly (some were causing problems with bind_rows)
#' 
#' @param df data frame of Statcast data
format_statcast_fields <- function(df) {
  require(dplyr)
  require(lubridate)
  
  factor.cols <- c("pitch_type", "events", "description", "zone", "game_type",
                   "stand", "p_throws", "home_team", "away_team", "type", "hit_location",
                   "bb_type", "inning_topbot", "launch_speed_angle", "pitch_name",
                   "if_fielding_alignment", "of_fielding_alignment", "barrel")
  
  int.cols <- c("batter", "pitcher", "balls", "strikes", "game_year", "on_1b",
                "on_2b", "on_3b", "outs_when_up", "inning", "iso_value", 
                "at_bat_number", "pitch_number")
  
  num.cols <- c("hit_distance_sc", "launch_speed", "launch_angle", "effective_speed",
                "game_pk", "woba_value", "woba_denom")
  
  char.cols <- c("player_name", "des", "umpire", "sv_id")
  
  df <- df %>% 
    mutate_at(factor.cols, factor) %>% 
    mutate_at(int.cols, as.integer) %>% 
    mutate_at(num.cols, as.numeric) %>% 
    mutate_at(char.cols, as.character) %>% 
    mutate_at("game_date", ymd) %>%
    mutate_at(vars(starts_with("release_")), as.numeric) %>%
    mutate_at("spin_dir", as.logical) %>%
    mutate_at(vars(matches("_deprecated")), as.logical) %>%
    mutate_at(vars(starts_with("pfx_")), as.numeric) %>%
    mutate_at(vars(matches("hc_[xy]")), as.numeric) %>%
    mutate_at(vars(matches("pos[123456789]_person_id")), as.character) %>%
    mutate_at(vars(starts_with("plate_")), as.numeric) %>%
    mutate_at(vars(matches("\\bv[xyz]0$")), as.numeric) %>%
    mutate_at(vars(matches("\\ba[xyz]$")), as.numeric) %>%
    mutate_at(vars(matches("\\bsz_[(bot)(top)]")), as.numeric) %>%
    mutate_at(vars(matches("estimated_")), as.numeric) %>%
    mutate_at(vars(matches("_score")), as.integer)
  
  return(df)
}

path <- "/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/data/"
if (!file.exists(paste0(path, "last_updates.csv"))) {
  reset_last_updates_file()
}

update_data(path=path)
