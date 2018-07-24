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
