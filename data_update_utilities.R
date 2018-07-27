#' This file contains useful functions for the data update process. It should
#' be sourced at the beginning of those files

#' Get the year of the current MLB season
#'
#' if 4/8 or later, assume regular season is in progress and use this year;
#' otherwise, use last year
#' 
#' this is pretty conservative -- want to avoid getting errors trying to pull 
#' Statcast data before season starts
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

#' make sure directory path ends in "/"
format_directory_path <- function(path) {
  require(stringr, quietly=TRUE, warn.conflicts=FALSE)
  if (str_sub(path, -1) != "/") {
    path <- paste0(path, "/")
  }
  return(path)
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
