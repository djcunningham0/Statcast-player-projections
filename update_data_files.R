update_data <- function() {
  update_crosswalk()
  update_speed_scores()
}

#' update mlb_player_id_crosswalk.rds
update_crosswalk <- function(path="/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/data/") {
  require(readr)
  require(dplyr)
  
  url <- "http://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv"
  crosswalk <- read_csv(url, col_types=cols()) %>% 
    # only keep if they have at least one of the IDs I'll be using
    filter(!(is.na(key_mlbam) & is.na(key_bbref) & is.na(key_fangraphs)))
  
  saveRDS(crosswalk, file=paste0(path, "mlb_player_id_crosswalk.rds"))
  
  # stamp today's date in the last update file
  register_updates("crosswalk", path=path)
}

#' update the speed_scores.rds
update_speed_scores <- function(start_year=2015, end_year=format(as.Date(Sys.Date()), "%Y"), agg=FALSE,
                                path="/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/data/") {
  require(readr)
  
  # scrape speed scores and update the RDS file
  speed_scores <- scrape_speed_scores(start_year=start_year, end_year=end_year, agg=agg)
  saveRDS(speed_scores, file=paste0(path, "speed_scores.rds"))
  
  # stamp today's date in the last update file
  register_updates("speed_scores", path=path)
}

#' basically a copy of baseballr::fg_bat_leaders but adds fangraphs ID only includes Spd
scrape_speed_scores <- function(start_year=2018, end_year=start_year, agg=FALSE, which="Spd") {
  require(xml2)
  require(rvest)
  require(dplyr)
  require(stringr)
  
  if (which == "Spd") {
    type_list <- "2,60"  # 2=year, 60=Spd
  }
  else {
    type_list == ""  # need to update this later
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
  # links <- links[c(TRUE, FALSE)]  # keep every other link (first is player, next is team)
  links <- links[str_detect(links, "playerid=")]  # only keep the player links
  
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
  
  # use crosswalk to add mlb and bbref IDs
  file <- "/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/data/mlb_player_id_crosswalk.rds"
  crosswalk <- readRDS(file) %>% 
    select(key_mlbam, key_bbref, key_fangraphs)
  out <- out %>% 
    left_join(crosswalk, by="key_fangraphs") %>% 
    select(Name, key_mlbam, key_bbref, key_fangraphs, position, Team, everything()) %>% 
    # format some values
    mutate_at("Season", as.integer) %>% 
    mutate_at("Spd", as.numeric)
  
  return(out)
}

#' record today's date in last_updates.cs
register_updates <- function(which, path) {
  update_df <- read_csv(paste0(path, "last_updates.csv"), col_types=cols())
  update_df[,which] <- Sys.Date()
  write_csv(update_df, paste0(path, "last_updates.csv"))
}

update_data()
