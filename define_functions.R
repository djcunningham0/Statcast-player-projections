#'
#' @param df data frame of Statcast data
#' @param lw vector of linear weights
#' 
#' @return a data frame with all of the necessary columns (plus some extras)
#' 
format_data_frame <- function(df, lw=NULL, lw_multiplier=NULL) {
  require(dplyr)
  
  if (is.null(lw)) {
    tmp <- set_linear_weights()
    lw <- tmp$lw
    lw_multiplier = tmp$multiplier
  }
  
  # create new data frame with possibly relevant columns
  # some are actually used for analysis, some are just kept for context when viewing the data
  keep <- c("game_date",
            "game_year",
            "game_month",
            "player_name",
            "batter",
            "pitcher",
            "events",
            "des",
            "game_type",
            "stand",
            "p_throws",
            "home_team",
            "away_team",
            "hit_location",
            "bb_type",
            "on_1b",
            "on_2b",
            "on_3b",
            "outs_when_up",
            "inning",
            "inning_topbot",
            "hc_x",
            "hc_y",
            "hit_distance_sc",
            "launch_speed",
            "launch_angle",
            "launch_speed_angle",
            "babip_value",
            "iso_value",
            "if_fielding_alignment",
            "of_fielding_alignment",
            "estimated_ba_using_speedangle",
            "estimated_woba_using_speedangle")
  
  batted <- df %>% 
    select(keep) %>% 
    rename("key_mlbam" = "batter",
           "Season" = "game_year") %>% 
    # only keep regular season games
    filter(game_type == "R") %>% 
    # calculate spray angle
    # reference: https://www.fangraphs.com/tht/research-notebook-new-format-for-statcast-data-export-at-baseball-savant/
    mutate(spray_angle = round((atan((hc_x-125.42)/(198.27-hc_y))*180/pi*.75),1)) %>% 
    # remove any rows with NA values in key columns
    filter(!is.na(launch_speed),
           !is.na(launch_angle),
           !is.na(spray_angle)) %>% 
    # identify infield shifts and separate by L/R
    #  - shifts are defined as three infielders on one side of second base
    #  - need to separate by L/R because that determines which side of second base
    #  - I'm not including the "Strategic" category because is not precisely defined
    mutate(shift = factor(case_when(if_fielding_alignment == "Infield shift" ~ paste0(stand,"_Infield shift"),
                                    TRUE ~ "Standard"))) %>% 
    # classify events into out/single/double/triple/HR
    mutate(babip_iso = paste0(babip_value,"_",iso_value),
           class = factor(case_when(babip_iso == "0_0" ~ "out",
                                    babip_iso == "1_0" ~ "single",
                                    babip_iso == "1_1" ~ "double",
                                    babip_iso == "1_2" ~ "triple",
                                    babip_iso == "0_3" ~ "home_run"))) %>% 
    # add linear weight values
    # linear weights reference: https://www.fangraphs.com/library/principles/linear-weights/
    # values from https://www.fangraphs.com/guts.aspx?type=cn
    mutate(linear_weight = round(case_when(class == "single" ~ lw["single"],
                                           class == "double" ~ lw["double"],
                                           class == "triple" ~ lw["triple"],
                                           class == "home_run" ~ lw["home_run"],
                                           TRUE ~ lw["out"]), 3)) %>% 
    # calculate MLB's xwOBA with this field (rename so it works with functions later)
    mutate(mlbx_linear_weight = estimated_woba_using_speedangle / lw_multiplier)
  
  # relevel shift so "Standard" is reference level
  batted$shift <- batted$shift %>% 
    relevel("Standard")
  
  # relevel class so "out" is reference level
  batted$class <- batted$class %>% 
    relevel("out")
  
  # add speed scores pulled from FanGraphs
  batted <- add_speed_scores(batted)
  
  return(batted)
}

#' @param years vector of years (average coefficients across these years)
#' 
#' @return linear weight values and the multiplier to convert to OBP scale
#' 
set_linear_weights <- function(years=2015:2017) {
  # add linear weight values
  # linear weights reference: https://www.fangraphs.com/library/principles/linear-weights/
  # values from https://www.fangraphs.com/guts.aspx?type=cn
  # currently using 2017 values
  require(dplyr)
  
  lw.df <- readRDS("./data/linear_weight_coefs.rds") %>% 
    filter(season %in% years)
  
  multiplier <- mean(lw.df$woba_scale)
  # out,single,double,triple,home_run,walk,HBP
  lw <- with(lw.df, c(0, mean(w1B), mean(w2B), mean(w3B), mean(wHR), mean(wBB), mean(wHBP))/multiplier)
  names(lw) <- c("out","single","double","triple","home_run","walk","HBP")
  
  return(list(lw=lw, multiplier=multiplier))
}

#' 
#' @param prefix the prefix for the column names (e.g., multinom, rf, knn, etc.)
#' @param df data frame of batted balls to add prediction columns to
#' @param probs predicted probabilities of each class (single, double, etc.)
#' @param lw vector of linear weight values
#' 
#' @return df with columns for predicted linear weight and probability of each hit type
#' 
add_preds_from_probs <- function(df, prefix, probs, lw=NULL) {
  if (is.null(lw)) {
    tmp <- set_linear_weights()
    lw <- tmp$lw
  }
  
  # predict linear weights and add to df
  df[,paste0(prefix,"_linear_weight")] <- predict_lw_from_probs(probs, lw)
  
  # add probs for 1B, 2B, 3B, HR
  for (class in c("single","double","triple","home_run")) {
    df[,paste0(prefix,"_",class)] <- probs[,class]
  }
  
  return(df)
}

#' 
#' @param probs an array of probabilities for out, single, double, etc.
#' @param lw vector of linear weights
#' 
#' @return vector of predicted linear weights
#' 
predict_lw_from_probs <- function(probs, lw) {
  classes <- c("out","single","double","triple","home_run")
  return(as.vector(probs[,classes] %*% lw[classes]))  # w/o as.vector it returns a matrix
}

#' 
#' @param df data frame (batted)
#' @param lw.prefixes vector of prefixes for models with linear weight predictions
#' @param full.prefixes vector of prefixes for models with full predictions
#' @param by_month set to TRUE to group data by year and month
#' 
#' @return a data frame grouped by player and year with linear weights and predicted linear weights
#' 
group_weights_by_year <- function(df, lw.prefixes=NULL, full.prefixes=NULL, by_month=FALSE) {
  require(dplyr)
  
  if (is.null(lw.prefixes)) { lw.prefixes <- get_prefixes(df, type="lw") }
  if (is.null(full.prefixes)) { full.prefixes <- get_prefixes(df, type="full") }
  
  cols <- c()
  for (pre in lw.prefixes) {
    cols <- c(cols, paste0(pre,"_linear_weight"))
    if (pre %in% full.prefixes) {
      cols <- c(cols, paste0(pre,"_",c("single","double","triple","home_run")))
    }
  }
  
  if (by_month==TRUE) {
    grouping_cols <- c("key_mlbam","Season","game_month","Spd")
  } else {
    grouping_cols <- c("key_mlbam","Season","Spd")
  }
  
  cols <- c(grouping_cols, "linear_weight", "mlbx_linear_weight", cols)
  weights.df <- df %>% 
    select(cols) %>% 
    group_by_at(grouping_cols) %>% 
    summarize_all(sum)
  
  return(weights.df)
}

#' Combine weights.df with true stats over the same time frame
#' 
#' @param weights.df data frame of weights from group_weights_by_year
#' @param lw.prefixes vector of prefixes for models with linear weight predictions
#' @param lw vector of linear weight values
#' @param lw_multiplier scaling constant for wOBA
#' @param current_season if TRUE, use current season summary file; otherwise, completed seasons file
#' 
#' @return a data frame grouped by player and year with predicted linear weights and wOBA
#' 
add_preds_to_yearly_data <- function(weights.df, lw.prefixes=NULL, lw=NULL,
                                     lw_multiplier=NULL, current_season=FALSE) {
  if (is.null(lw)) {
    tmp <- set_linear_weights()
    lw <- tmp$lw
    lw_multiplier <- tmp$multiplier
  }
  
  if (is.null(lw.prefixes)) { lw.prefixes <- get_prefixes(weights.df, type="lw") }
  
  if (current_season) {
    batting.df <- readRDS("./data/current_season_summary.rds")
  } else {
    minYear <- min(weights.df$Season)
    maxYear <- max(weights.df$Season)
    
    batting.df <- readRDS("./data/completed_seasons_summary.rds") %>% 
      filter(Season %in% minYear:maxYear)
  }
  
  # join batting.df and weights.df
  batting.df <- batting.df %>% 
    full_join(weights.df, by=c("key_mlbam", "Season"))
  
  # add predicted wOBA values from models
  # use true values for AB, BB, etc.
  for (pre in lw.prefixes) {
    batting.df[,paste0(pre,"_wOBA")] <- with(batting.df, (lw_multiplier*(batting.df[,paste0(pre,"_linear_weight")] + 
                                                                           lw["walk"]*(BB-IBB) + lw["HBP"]*HBP) / PA))
  }
  
  return(batting.df)
}


#' 
#' @param batting.df data frame from add_preds_to_yearly_data
#' 
#' @return lagged data frame with year and previous year values in each row
#' 
lag_yearly_data <- function(batting.df) {
  require(dplyr)
  require(stringr)
  
  batting_lagged <- batting.df %>% 
    mutate(next_year = Season + 1)
  
  cur.df <- batting_lagged %>% select(-next_year)
  next.df <- batting_lagged %>% select(-Season)
  batting_lagged <- cur.df %>%
    inner_join(next.df, by=c("key_mlbam", "Season"="next_year"))
  colnames(batting_lagged) <- str_replace(colnames(batting_lagged), ".x", "")
  colnames(batting_lagged) <- str_replace(colnames(batting_lagged), ".y", ".prev")
  
  return(batting_lagged)
}


#' any columns containing "_linear_weight" have linear weight predictions for that model
#' any columns containing "_home_run" have full predictions for that model
#' 
#' @param batted balls data frame
#' @param type "lw" for linear weight predictions, "full" for full predictions
#' 
#' @return a vector of model prefixes
#' 
get_prefixes <- function(df, type="") {
  if (type != "lw" & type != "full") {
    print("Must set type to 'lw' or 'full'.")
    return(NULL)
  }
  
  col_names <- colnames(df)
  if (type=="lw") { 
    indices <- grep("_linear_weight", col_names) 
  } else { 
    indices <- grep("_home_run", col_names) 
  }
  
  fullnames <- col_names[indices]
  prefix <- sapply(strsplit(fullnames,"_"),'[',1)
  
  return(prefix)
}

#' speed scores available from FanGraphs:
#' https://www.fangraphs.com/library/offense/spd/ 
#' 
#' speed_scores.rds is updated by functions in update_data_files.R
#' 
#' @param batted data frame of batted balls
#' 
#' @return the batted data frame with FanGraphs speed scores added
add_speed_scores <- function(batted) {
  require(readr)
  require(dplyr)
  
  speed_scores <- readRDS("./data/speed_scores.rds")
  
  # group by MLB ID and year in case there are multiple entries for a player per year 
  # (not sure if this happens -- don't think it actually does)
  # (should really do a weighted average of the player's two scores, but this is good enough)
  speed_scores <- speed_scores %>% 
    group_by(key_mlbam, Season) %>% 
    summarize(Spd = mean(Spd))
  
  batted <- batted %>% 
    left_join(speed_scores, by=c("key_mlbam", "Season"))
  
  return(batted)
}

#' Generate player projections using the Marcel the Monkey framework described by Tom Tango:
#' http://www.tangotiger.net/archives/stud0346.shtml
#' 
#' Pass in a data frame with predictions from the Statcast models to return projections from
#' those totals. If no data frame is passed in, regular Marcel projections are returned.
#' 
#' @param year the year to create forecasts for
#' @param pred_df data frame containing predicted 1B, 2B, 3B, HR totals (if null, use true stats)
#' @param model_prefix the prefix of the model to be used from pred_df
#' @param lw_years year(s) to get linear weight coefficients for (default: average of past 3 years)
#' @param verbose if TRUE, print message if a year isn't included in pred_df
#' 
#' @return a data frame with Marcel projections for all players from one method
marcel_projections <- function(year, pred_df=NULL, model_prefix=NULL, lw_years=(year-3):(year-1), 
                               verbose=FALSE) {
  require(dplyr)
  
  if (!is.null(pred_df) & is.null(model_prefix)) {
    print("Must specify which model's predictions to use in 'model' parameter.")
    return(NULL)
  }
  
  # get league average data by year for past three years
  past.years <- c(year-3, year-2, year-1)
  batting.sub <- readRDS("./data/completed_seasons_summary.rds") %>%  # this file is updated in update_data_files.R
    filter(Season %in% past.years,
           # exclude pitchers from league average hitting stats
           position != "P")
  
  ### decision point: should league averages be computed from actual stats, or expected  ###
  ###                 stats from models? currently using actual stats                    ###
  cols <- c("AB", "PA", "H", "X1B", "X2B", "X3B", "HR", "R", "RBI", "BB", "IBB", "SO",
            "HBP", "SF", "SH", "SB", "CS")
  league.total <- batting.sub %>% 
    group_by(Season) %>% 
    summarize_at(cols, sum) %>% 
    ungroup() %>% 
    arrange(desc(Season))  # year-1, then year-2, then year-3
  
  # for easy lookup later on:
  rownames(batting.sub) <- with(batting.sub, paste0("_",key_mlbam,"_",Season))
  
  # create projections for all players (non-pitchers) with batting data in previous season
  marcel.df <- batting.sub %>% 
    filter(Season == year-1) %>% 
    select(Season, Name, key_mlbam, Age) %>% 
    # now add 1 since we're projecting for next year
    mutate(Season = Season + 1,
           Age = Age + 1)
  
  if (is.null(pred_df)) {
    # only use true stats
    # need to do this with subset instead of dplyr to keep row names
    prev1 <- subset(batting.sub, Season == year-1)
    prev2 <- subset(batting.sub, Season == year-2)
    prev3 <- subset(batting.sub, Season == year-3)
  } else {
    # remove pitchers
    pred_df <- pred_df %>% 
      filter(position != "P")
    
    # do some formatting so we have the same columns as batting.sub for later operations
    pred.cols <- c("Name", "key_mlbam", "position", "Team", "Season", "Age",
                   paste0(model_prefix,"_",c("single","double","triple","home_run")),
                   # use all the true values except the model is predicting
                   cols[!(cols %in% c("X1B", "X2B", "X3B", "HR"))])
    pred_df <- pred_df %>% 
      select(pred.cols)
    
    colnames(pred_df)[colnames(pred_df) == paste0(model_prefix,"_single")] <- "X1B"
    colnames(pred_df)[colnames(pred_df) == paste0(model_prefix,"_double")] <- "X2B"
    colnames(pred_df)[colnames(pred_df) == paste0(model_prefix,"_triple")] <- "X3B"
    colnames(pred_df)[colnames(pred_df) == paste0(model_prefix,"_home_run")] <- "HR"
    rownames(pred_df) <- with(pred_df, paste0("_",key_mlbam,"_",Season))
    
    # (tried doing this with ifelse and it didn't work)
    pred.years <- unique(pred_df$Season)
    if ((year-1) %in% pred.years) { 
      prev1 <- subset(pred_df, Season == year-1)
    } else { 
      prev1 <- subset(batting.sub, Season == year-1)
    }
    if ((year-2) %in% pred.years) { 
      prev2 <- subset(pred_df, Season == year-2)
    } else { 
      prev2 <- subset(batting.sub, Season == year-2)
    }
    if ((year-3) %in% pred.years) { 
      prev3 <- subset(pred_df, Season == year-3)
    } else { 
      prev3 <- subset(batting.sub, Season == year-3)
    }
    
    if (verbose==TRUE){
      for (i in 1:3) {
        if (!((year-i) %in% pred.years)) { 
          print(paste0(year-i," from observed stats")) 
        }
      }
    }
  }
  
  # Expected PA: (.5*prev1) + (.1*prev2) + 200
  # If data is missing (player didn't play that year), count as 0
  PA1 <- ifelse(!is.na(prev1[paste0("_",marcel.df$key_mlbam,"_",year-1), "PA"]),
                prev1[paste0("_",marcel.df$key_mlbam,"_",year-1), "PA"],
                0)
  PA2 <- ifelse(!is.na(prev2[paste0("_",marcel.df$key_mlbam,"_",year-2), "PA"]),
                prev2[paste0("_",marcel.df$key_mlbam,"_",year-2), "PA"],
                0)
  PA3 <- ifelse(!is.na(prev3[paste0("_",marcel.df$key_mlbam,"_",year-3), "PA"]),
                prev3[paste0("_",marcel.df$key_mlbam,"_",year-3), "PA"],
                0)
  marcel.df$xPA <- 0.5*PA1 + 0.1*PA2 + 200
  
  # Expected values for stats except PAs:
  #  - weight value from previous 3 seasons as 5/4/3 (:= weighted.val)
  #  - weight PAs from previous 3 seasons as 5/4/3 (:= weighted.PAs)
  #  - prorate league average stats to player's number of PAs, then weight as 5/4/3
  #  - calculate player's league average rate as weighted value / # of PAs
  #  - prorate this leave average to 1200 ABs (:= weighted.avg)
  #  - calculate projected rate: (weighted.avg + weighted.val) / (1200 + weighted.PAs)
  #  - multiply projected rate by projected PAs
  #  - age adjustment: multiply projected values by 1 + [(29 - age) * .006]
  #     - 0.006 for age < 29, 0.003 for age > 29
  
  weighted.PAs <- 5*PA1 + 4*PA2 + 3*PA3
  age.constant <- ifelse(marcel.df$Age < 29, 0.006, 0.003)
  age.adjust <- (29 - marcel.df$Age) * age.constant
  for (val in cols[cols != "PA"]) {
    val1 <- ifelse(!is.na(prev1[paste0("_",marcel.df$key_mlbam,"_",year-1), val]),
                   prev1[paste0("_",marcel.df$key_mlbam,"_",year-1), val],
                   0)
    val2 <- ifelse(!is.na(prev2[paste0("_",marcel.df$key_mlbam,"_",year-2), val]),
                   prev2[paste0("_",marcel.df$key_mlbam,"_",year-2), val],
                   0)
    val3 <- ifelse(!is.na(prev3[paste0("_",marcel.df$key_mlbam,"_",year-3), val]),
                   prev3[paste0("_",marcel.df$key_mlbam,"_",year-3), val],
                   0)
    weighted.val <- 5*val1 + 4*val2 + 3*val3
    weighted.avg <- (5*PA1*league.total[1,val] / league.total[1,"PA"] 
                   + 4*PA2*league.total[2,val] / league.total[2,"PA"]
                   + 3*PA3*league.total[3,val] / league.total[3,"PA"])
    weighted.avg <- as.numeric(weighted.avg / weighted.PAs * 1200)
    rate <- (weighted.avg + weighted.val) / (1200 + weighted.PAs)
    
    marcel.df[, paste0("x",val)] <- rate * marcel.df$xPA * (1 + age.adjust)
  }
  
  # compute additional projected stats
  marcel.df$xAB   <- with(marcel.df, xPA - (xBB+xSF+xSH+xHBP))
  marcel.df$xH    <- with(marcel.df, xX1B+xX2B+xX3B+xHR)
  marcel.df$xBA   <- with(marcel.df, xH/xAB)
  marcel.df$xOBP  <- with(marcel.df, (xH+xBB+xHBP)/xPA)
  marcel.df$xSLG  <- with(marcel.df, (xX1B + 2*xX2B + 3*xX3B + 4*xHR)/xAB)
  marcel.df$xOPS  <- with(marcel.df, xOBP+xSLG)
  
  tmp <- set_linear_weights(lw_years)
  lw <- tmp$lw
  lw_multiplier <- tmp$multiplier
  marcel.df$xwOBA <- with(marcel.df, (lw_multiplier * (lw["walk"]*(xBB-xIBB) + lw["HBP"]*xHBP + 
                                                         lw["single"]*xX1B + lw["double"]*xX2B + 
                                                         lw["triple"]*xX3B + lw["home_run"]*xHR) / xPA))
  
  # remove rows if we couldn't calculate Marcel projections 
  #   - (e.g., player had data for previous year but 0 PA -- see kiermke01 2014)
  marcel.df <- marcel.df %>% 
    filter(!is.nan(xwOBA))
  
  if (!is.null(pred_df)) {
    # only need the projections that will be different from standard Marcel
    keep <- c("xX1B", "xX2B", "xX3B", "xHR", "xH", "xBA", "xOBP", "xSLG", "xOPS", "xwOBA")
    marcel.df <- marcel.df[,c("Name", "key_mlbam","Season","Age",keep)]
  }
  return(marcel.df)
}

#' Create a data frame containing Marcel projections from multiple Statcast models. One row
#' per player with columns for projected stats from standard Marcel and Statcast model Marcel.
#' Optionally include true stats in the data frame.
#' 
#' @param year the year to create forecasts for
#' @param pred_df data frame containing predicted 1B, 2B, 3B, HR totals (if null, use true stats)
#' @param prefixes vector of prefixes of models from pred_df to use for Marcel projections
#' @param lw_years year(s) to get linear weight coefficients for (default: average of past 3 years)
#' @param include_true_stats if TRUE, include players' true stats in the returned data frame
#' @param AB_cutoff include players with at least this many ABs (only relevant if include_true_stats is TRUE)
#' 
#' @return a data frame with one row per each player and columns for each Marcel projection
get_marcel_eval_df <- function(year, pred_df=NULL, prefixes=get_prefixes(pred_df,type='full'),
                               lw_years=(year-3):(year-1), include_true_stats=TRUE, AB_cutoff=0) {
  marcel <- marcel_projections(year, lw_years=lw_years)
  
  # combine true stats and Marcel projections
  if (include_true_stats) {
    df <- readRDS("./data/completed_seasons_summary.rds") %>% 
      right_join(marcel, by=c("Name", "key_mlbam", "Season", "Age")) %>% 
      select(-key_fangraphs)
  } else {
    df <- marcel
  }
  
  # now add adjusted Marcel projections
  if (!is.null(pred_df)) {
    for (pre in prefixes) {
      marcel.adj <- marcel_projections(year=year, pred_df=pred_df, model_prefix=pre, lw_years=lw_years)
      df <- df %>% 
        left_join(marcel.adj, by=c("Name", "key_mlbam", "Season", "Age"))
      colnames(df) <- gsub(".x","",colnames(df), fixed=TRUE)
      colnames(df) <- gsub(".y",paste0("_",pre),colnames(df), fixed=TRUE)
    }
  }
  
  if ((AB_cutoff > 0) & include_true_stats) {
    df <- df %>% 
      filter(AB >= AB_cutoff)
  }
  
  return(df)
}


#' Get the true stats for all players in a given year (regular season must be completed)
#' 
#' completed_seasons.rds is updated by functions in update_data_files.R
#' 
#' @param year completed season to get stats from
#' @param playerIDs vector of mldam IDs to include
#' 
get_true_stats <- function(year, playerIDs=NULL) {
  # 'vals' will be true stats for that year
  vals <- readRDS("./data/completed_seasons_summary.rds") %>% 
    filter(Season == year)
  
  if (!is.null(playerIDs)) { 
    vals <- vals %>% 
      filter(key_mlbam %in% playerIDs)
  }
  
  return(vals)
}

#' Create multiple scatterplots of Marcel projected stats with their true values
#' (Note: does not return anything, just prints the plot)
#' 
#' @param eval_df data frame from get_marcel_eval_df
#' @param model_prefix prefix of the model to use from eval_df ("" for standard Marcel)
#' @param stats which stats to plot
#' @param model_desc a description for the model (a string)
#' @param xlabs a vector of labels to put on the x-axes
#' @param titles a vector of titles for the plots
#' @param center_title if TRUE, center the title
#' @param title_size text size for the title
#' @param text_size text size for other plot elements
#' 
marcel_eval_plot <- function(eval_df, model_prefix="", stats=c("OBP","SLG","OPS","wOBA"),
                             model_desc=NULL, xlabs=NULL, titles=NULL,
                             center_title=FALSE, title_size=NULL, text_size=NULL) {
  require(gridExtra)
  
  if (model_prefix!="") { model_prefix <- paste0("_",model_prefix) }
  if (is.null(model_desc)) { ylabs <- paste0("x",stats,model_prefix) }
  else { ylabs <- paste0(model_desc," ",stats)}
  if (is.null(xlabs)) { xlabs <- stats }
  if (is.null(titles)) { titles <- paste0(max(eval_df$Season)," ",ylabs," vs. ",xlabs) }
  
  n <- length(stats)
  if (n > 4) {
    return("Too many statistics listed. Limit is 4.")
  }
  if (!(n==length(xlabs) & n==length(ylabs) & n==length(titles))) {
    return("Number of labels or titles does not match number of statistics.")
  }
  
  out <- list()
  for (i in 1:4) {
    if (n >= i) {
      p <- create_scatterplot(data=eval_df, x.col=stats[i], y.col=paste0("x",stats[i],model_prefix),
                             xlab=xlabs[i], ylab=ylabs[i], plotTitle=titles[i],
                             center_title=center_title, title_size=title_size, text_size=text_size)
      out[[i]] <- p
    }
  }

  nrow <- ifelse(n<=2, 1, 2)
  ncol <- ifelse(n==1, 1, 2)
  do.call("grid.arrange", c(out, nrow=nrow, ncol=ncol))
}

#' Add steamer projections to Marcel eval df
#' 
#' @param eval_df data frame from get_marcel_eval_df
#' @param steamer_df data frame containing Steamer projections
#' 
#' @return eval_df with steamer projections added
#' 
add_steamer_to_eval_df <- function(eval_df, steamer_df) {
  # remove steamer columns if they're already there
  eval_df <- eval_df[, !(grepl("steamer", colnames(eval_df)))]
  
  # format the Steamer data frame to keep the columns we need
  steamer_df <- steamer_df %>% 
    mutate(OPS = OBP + SLG) %>% 
    rename("BA" = "AVG") %>% 
    select(key_mlbam, PA, X1B, X2B, X3B, HR, BA, OBP, SLG, OPS, wOBA)
  colnames(steamer_df) <- paste0("x",colnames(steamer_df),"_steamer")
  
  df <- eval_df %>% 
    left_join(steamer_df, by=c("key_mlbam" = "xkey_mlbam_steamer"))
  return(df)
}

#' Compute some summary statistics for the projection methods (correlation, MAE, and RMSE).
#' 
#' @param eval_df data frame from get_marcel_eval_df
#' @param stat which stat to evaluate (e.g., "wOBA", "OPS", etc.)
#' 
#' @return data frame with one row per complete projection method and columns for eval metrics
#' 
create_eval_summary <- function(eval_df, stat="wOBA") {
  require(ModelMetrics)
  
  # baseline is the vector of true values
  baseline <- eval_df[,stat]
  
  # estimates are the vectors of the projected values
  estimates <- eval_df[,grep(paste0("x",stat),colnames(eval_df))]
  colnames(estimates)[colnames(estimates)==paste0("x",stat)] <- paste0("x",stat,"_marcel")
  
  # n is number of different projections we have
  n <- dim(estimates)[2]
  cor.vals <- rep(0, n)
  mae.vals <- rep(0, n)
  rmse.vals <- rep(0, n)
  methods <- sapply(strsplit(colnames(estimates),"_"),'[',2)
  
  # get correlation, MAE, and RMSE for each projection method
  for (i in 1:n) {
    cor.vals[i] <- cor(baseline, estimates[,i])
    mae.vals[i] <- mae(baseline, estimates[,i])
    rmse.vals[i] <- rmse(baseline, estimates[,i])
  }
  
  summary.df <- data.frame(method=methods, cor=cor.vals, mae=mae.vals, rmse=rmse.vals)
  attr(summary.df, "stat") <- stat  # keep track of which stat the summary is for (used in plot subtitle later)
  return(summary.df)
}

#' Scale the projection evaluation data frame to more interpretable values
#' 
#' @param summary_df data frame from create_eval_summary
#' @param marcel_at_zero if TRUE, scale so that all standard Marcel values are zero;
#'                       otherwise, the minimum value will be set ot zero in each category
scale_eval_summary <- function(summary_df, marcel_at_zero=TRUE) {
  if (marcel_at_zero==TRUE) {
    baseline <- subset(summary_df, method=="marcel")
  }
  else { baseline <- summary_df }
  
  summary_df$cor <- scale_values_01(summary_df$cor, zero_val=min(baseline$cor))
  summary_df$mae <- scale_values_01(-summary_df$mae, zero_val=min(-baseline$mae))
  summary_df$rmse <- scale_values_01(-summary_df$rmse, zero_val=min(-baseline$rmse))
  
  return(summary_df)
}

#' Scale values so they fall between 0 and 1
#' 
#' @param x numeric vector
#' @param zero_val number that should be scaled to zero (default is minimum)
scale_values_01 <- function(x, zero_val=min(x)) { (x-zero_val)/(max(x)-zero_val) }


#' Convert columns of eval_summary to rows for easier plotting.
#' 
#' @param summary_df data frame from create_eval_summary (or scale_eval_summary)
reshape_eval_summary <- function(summary_df) {
  require(tidyr)
  return(gather(summary_df, metric, value, -method))
}

#' Create a plot to visualize the performance of each projection system
#' 
#' @param summary_df data frame from create_eval_summary
#' @param which vector of projection methods from summary_df to include in the plot
#' @param names vector of names to show in the legend (must be same length and same order as which)
#' @param scale if TRUE, 
#' 
#' @return a ggplot object
plot_projection_summary <- function(summary_df, which=summary_df$method, names=NULL,
                                    scale=TRUE, metrics=c("cor", "mae", "rmse"), 
                                    point_size=3, center_title=TRUE,
                                    title_size=16, text_size=12,
                                    plot.title="Relative Accuracy of Projections",
                                    subtitle=paste0("(", attr(summary_df, "stat"), ")")) {
  require(dplyr)
  require(forcats)
  
  # add subtitle if specified
  if (!is.null(subtitle) && !(subtitle %in% c("", "()"))) {
    plot.title <- paste0(plot.title, "\n", subtitle)
  }
  
  if (scale) {
    summary_df <- scale_eval_summary(summary_df)
  }
  
  # only keep the methods that we want to plot
  summary_df <- reshape_eval_summary(summary_df) %>% 
    filter(method %in% which, metric %in% metrics) %>% 
    mutate_at("method", factor) %>% 
    mutate_at("metric", factor)
  
  summary_df$metric <- fct_recode(summary_df$metric, Correlation = "cor", MAE = "mae", RMSE = "rmse")
  
  # rename the factor levels if specified
  if (!is.null(names)) {
    if (length(which) != length(names)) {
      print("Length of names does not match number of methods. Not attempting to rename.")
    } else {
      for (i in 1:length(which)) {
        levels(summary_df$method)[levels(summary_df$method) == which[i]] <- names[i]
      }
    }
  }
  
  p <- (ggplot(summary_df, aes(x=metric, y=value, color=method))
        + geom_point(size=point_size)
        + labs(x="", y="scaled value", title=plot.title)
        + theme(legend.title=element_blank())
        + theme(legend.key.size=unit(text_size/2,'mm'))
        + theme(text=element_text(size=text_size))
        + theme(plot.title=element_text(size=title_size))
  )
  if (center_title) {
    p <- p + theme(plot.title=element_text(hjust=0.5))
  }
  return(p)
}

#' make sure directory path ends in "/"
#' 
#' @param path a path to a directory (string)
#' 
#' @return the same path ending in "/"
#' 
format_directory_path <- function(path) {
  require(stringr, quietly=TRUE)
  if (str_sub(path, -1) != "/") {
    path <- paste0(path, "/")
  }
  return(path)
}

#' 
#' @param data data frame with data to plot
#' @param x.col name of column in data to use for x axis
#' @param y.col name of column in data to use for y axis
#' @param color.col name of column in data to use for point color
#' @param xlab x-axis label (default: x.col)
#' @param ylab y-axis label (default: y.col)
#' @param plotTitle title for plot (default: ylab vs. xlab)
#' @param includeCorrelation if TRUE, show correlation of x, y on second line of title
#' @param include_y_equal_x if TRUE, show y=x line
#' @param center_title if TRUE, center the title
#' @param title_size text size for title
#' @param text_size text size for other plot elements
#' @param point_size point size for scatterplot
#' @param point_alpha transparency amount for points
#' @param text.cols columns to concatenate for point text (useful if viewing with ggplotly)
#' @param colorBarTitle title for colorbar
#' 
#' @return a ggplot object
#' 
create_scatterplot <- function(data, x.col, y.col, color.col=NULL, xlab=substitute(x.col),
                               ylab=substitute(y.col), plotTitle=paste0(ylab," vs. ",xlab),
                               includeCorrelation=TRUE, include_y_equal_x=TRUE,
                               center_title=FALSE, title_size=NULL, text_size=NULL,
                               point_size=NULL, point_alpha=0.75,
                               text.cols=c("Name","Season"),
                               colorBarTitle=color.col) {
  require(ggplot2)
  
  text.format <- theme()
  if (center_title==TRUE) { text.format <- text.format + theme(plot.title=element_text(hjust=0.5)) }
  if (!is.null(title_size)) { text.format <- text.format + theme(plot.title=element_text(size=title_size)) }
  if (!is.null(text_size)) { text.format <- text.format + theme(text=element_text(size=text_size)) }
  
  if (is.null(point_size)) { point.spec <- geom_point(alpha=point_alpha) }
  else { point.spec <- geom_point(alpha=point_alpha, size=point_size) }
  
  x <- data[,x.col]
  y <- data[,y.col]
  if (includeCorrelation == TRUE) { plotTitle <- paste0(plotTitle,"\nCorrelation: ",
                                                        round(cor(x,y,use="pair"),3)) }
  
  # set point labels for using ggplotly
  label <- NULL
  for (col in text.cols) {
    if (!(col %in% colnames(data))) {
      print(paste0("Warning: ",col," not a valid column name for text.cols"))
      label <- ""
    }
  }
  if (is.null(label)) {
    label <- data[,text.cols[1]]
    for (i in 2:length(text.cols)) {
      label <- paste0(label,"_",data[,text.cols[i]])
    }
  }
  
  if (is.null(color.col)) {
    aes <- aes(x=x, y=y, text=label)
    colorbar.format <- scale_colour_gradient()
  } else {
    color <- data[,color.col]
    aes <- aes(x=x, y=y, color=color, text=label)
    colorbar.format <- scale_colour_gradientn(colours = terrain.colors(5),
                                              guide=guide_colorbar(title=colorBarTitle))
  }
  
  p <- (ggplot(data=data,aes)
        + point.spec
        + labs(x=xlab,y=ylab,title=plotTitle)
        + text.format
        + colorbar.format
  )
  
  if (include_y_equal_x == TRUE) {
    p <- p + geom_abline(slope=1, intercept=0, color='red', lty=2)
  }
  
  return(p)
}


#' Note: './knn cross validation.R' was used to test different values of k using cross
#' validation before fitting the model with this function
#' 
#' @param df data frame
#' @param k value of k to use
#' @param trainSize proportion of data to use for training the model
#' @param seed seed to set
#' @param type 'classification' to predict class probabilities or 'regression' to predict linear weights
#' 
#' @return a kNN classification model
#' 
fit_knn_model <- function(df, k, trainSize, seed=NULL, type="classification") {
  require(caret)
  
  if (!(type %in% c("classification","regression"))) {
    print("Must set type to 'classification' or 'regression'.")
    return(NULL)
  }
  
  if (!is.null(seed)) { set.seed(seed) }
  n <- dim(df)[1]
  which.train <- sample(1:n, floor(n*trainSize))
  training <- df %>% 
    select(which.train)
  
  fitCtrl <- trainControl(method = "none")
  
  if (type=="classification") {
    knnmod <- train(class ~ ., data=training[c("class","launch_speed","launch_angle","spray_angle")],
                    method="knn", trControl=fitCtrl, tuneGrid=expand.grid(k=k),
                    preProcess=c("scale","center"))
  }
  else {
    knnmod <- train(linear_weight ~ ., data=training[c("linear_weight","launch_speed","launch_angle","spray_angle")],
                    method="knn", trControl=fitCtrl, tuneGrid=expand.grid(k=k),
                    preProcess=c("scale","center"))
  }
  return(knnmod)
}
