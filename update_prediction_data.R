#' This file creates two data frames:
#'   1. Data frame of current year stats and predicted stats
#'   2. Data frame of past year stats and predicted stats
#' 
#' These data frames can be used to (1) evaluate in-season performance relative
#' to expected performance and (2) create Marcel projections.

source("./define_functions.R")  # this has functions for manipulating the raw data
source("./data_update_functions.R")  # this has a few useful utilities for getting correct years, etc.

#' wrapper function
update_all_preds <- function() {
  update_current_season_preds()
  update_completed_season_preds()
}


#' 
update_current_season_preds <- function(rf_model=TRUE, multinom_model=FALSE, knn_model=FALSE,
                                        rf_path="./models/rf.rds",
                                        multinom_path="./models/multinom.rds",
                                        knn_path="./models/multinom.rds") {
  require(dplyr, quietly=TRUE, warn.conflicts=FALSE)
  
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
  
  saveRDS(batting.df, file="./prediction_data/current_season_wOBA_preds.rds")
}


#' TO DO: combine these two functions (current and completed seasons) into one
update_completed_season_preds <- function(rf_model=TRUE, multinom_model=FALSE, knn_model=FALSE,
                                          rf_path="./models/rf.rds",
                                          multinom_path="./models/multinom.rds",
                                          knn_path="./models/multinom.rds") {
  require(dplyr, quietly=TRUE, warn.conflicts=FALSE)
  
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
  
  saveRDS(batting.df, file="./prediction_data/completed_season_wOBA_preds.rds")
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


update_all_preds()
