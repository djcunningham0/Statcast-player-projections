#' This file creates two data frames:
#'   1. Data frame of current year stats and predicted stats
#'   2. Data frame of past year stats and predicted stats
#' 
#' These data frames can be used to (1) evaluate in-season performance relative
#' to expected performance and (2) create Marcel projections.

setwd("/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/")
source("./define_functions.R")  # this has functions for manipulating the raw data
source("./data_update_utilities.R")  # this has a few useful utilities for getting correct years, etc.


#' wrapper function
update_all_preds <- function(out_path="/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/prediction_data/") {
  # these don't take too long to run, so I'm being lazy and just letting them run daily
  update_current_season_preds()
  update_completed_season_preds()
  update_marcel_projections()
}


#' 
update_current_season_preds <- function(rf_model=TRUE, multinom_model=FALSE, knn_model=FALSE,
                                        rf_path="./models/rf.rds",
                                        multinom_path="./models/multinom.rds",
                                        knn_path="./models/multinom.rds",
                                        out_path="/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/prediction_data/") {
  require(dplyr, quietly=TRUE, warn.conflicts=FALSE)
  
  print("Updating current season prediction data...")
  
  tmp <- set_linear_weights(get_current_season_year())
  lw <- tmp$lw
  lw_multiplier <- tmp$multiplier
  
  # read in current season Statcast batted ball data and format it for further manipulation
  batted <- readRDS("./data/current_season_statcast_batted_balls.rds") %>% 
    format_data_frame(lw=lw, lw_multiplier=lw_multiplier)
  
  # add predictions from the selected model(s)
  if (rf_model) {
    require(randomForest, quietly=TRUE, warn.conflicts=FALSE)
    batted <- add_model_preds(df=batted, lw=lw, model=rf_path, prefix="rf")
  }
  if (multinom_model) {
    require(nnet, quietly=TRUE, warn.conflicts=FALSE)
    batted <- add_model_preds(df=batted, lw=lw, model=multinom_path, prefix="multinom")
  }
  if (knn_model) {
    require(caret, quietly=TRUE, warn.conflicts=FALSE)
    batted <- add_model_preds(df=batted, lw=lw, model=knn_path, prefix="knn")
  }
  
  # group by player, then combine with true stats
  batting.df <- group_weights_by_year(batted) %>% 
    add_preds_to_yearly_data(lw=lw, lw_multiplier=lw_multiplier, current_season=TRUE)
  
  saveRDS(batting.df, file=paste0(out_path, "current_season_wOBA_preds.rds"))
  
  register_updates("current_season", out_path)
}


#' TO DO: combine these two functions (current and completed seasons) into one
update_completed_season_preds <- function(rf_model=TRUE, multinom_model=FALSE, knn_model=FALSE,
                                          rf_path="./models/rf.rds",
                                          multinom_path="./models/multinom.rds",
                                          knn_path="./models/multinom.rds",
                                          out_path="/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/prediction_data/") {
  require(dplyr, quietly=TRUE, warn.conflicts=FALSE)
  
  print("Updating completed seasons prediction data...")
  
  year <- get_last_completed_season_year()
  tmp <- set_linear_weights((year-2):year)
  lw <- tmp$lw
  lw_multiplier <- tmp$multiplier
  
  # read in completed season Statcast batted ball data and format it for further manipulation
  batted <- readRDS("./data/completed_seasons_statcast_batted_balls.rds") %>% 
    format_data_frame(lw=lw, lw_multiplier=lw_multiplier)
  
  # add predictions from the selected model(s)
  if (rf_model) {
    require(randomForest, quietly=TRUE, warn.conflicts=FALSE)
    batted <- add_model_preds(df=batted, lw=lw, model=rf_path, prefix="rf")
  }
  if (multinom_model) {
    require(nnet, quietly=TRUE, warn.conflicts=FALSE)
    batted <- add_model_preds(df=batted, lw=lw, model=multinom_path, prefix="multinom")
  }
  if (knn_model) {
    require(caret, quietly=TRUE, warn.conflicts=FALSE)
    batted <- add_model_preds(df=batted, lw=lw, model=knn_path, prefix="knn")
  }
  
  # group by player, then combine with true stats
  batting.df <- group_weights_by_year(batted) %>% 
    add_preds_to_yearly_data(lw=lw, lw_multiplier=lw_multiplier, current_season=FALSE)
  
  saveRDS(batting.df, file=paste0(out_path, "completed_seasons_wOBA_preds.rds"))
  
  register_updates("completed_seasons", out_path)
}


#' 
update_marcel_projections <- function(out_path="/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/prediction_data/") {
  require(dplyr, quietly=TRUE, warn.conflicts=FALSE)
  
  print("Updating player projection data...")
  
  batting.df <- readRDS("./prediction_data/completed_seasons_wOBA_preds.rds")
  
  all_years <- sort(unique(batting.df$Season))
  
  # we can do Statcast-enhanced Marcel projections starting the year after the 3rd up until the 
  # year after the last
  # TO DO: change this so it does partial Statcast-enhanced Marcel for 2017 
  # (will probably require updating how I'm creating some data files)
  projection_df <- c()
  for (year in (all_years[3]+1):(tail(all_years, 1)+1)) {
    marcel_df <- get_marcel_eval_df(year, lw_years=(year-3):(year-1), 
                                    pred_df=batting.df, include_true_stats=FALSE)
    
    projection_df <- bind_rows(projection_df, marcel_df)
  }
  
  saveRDS(projection_df, file=paste0(out_path, "marcel_projections.rds"))
  
  register_updates("marcel_projections", out_path)
}

#' Add predictions from a classification model to batted ball dataframe
#' 
#' @param df batted ball data frame
#' @param lw vector of linear weights
#' @param model file path for the saved model
#' @param prefix prefix for the model
#' 
#' @return data frame with model predictions added
#' 
add_model_preds <- function(df, lw, model, prefix) {
  mod <- readRDS(model)
  probs <- predict(mod, newdata=df, type="prob")
  probs <- as.matrix(probs)  # not necessary for most models, but doesn't hurt
  return(add_preds_from_probs(df=df, prefix=prefix, probs=probs, lw=lw))
}

path <- "/Users/Daniel/Documents/University of Chicago/thesis/Statcast linear weights/prediction_data/"
update_all_preds(out_path=path)
