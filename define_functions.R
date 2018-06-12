#'
#' @param df data frame (should be created from CSV file)
#' @param lw vector of linear weights
#' 
#' @return a data frame with all of the necessary columns (plus some extras)
#' 
format_data_frame <- function(df,lw) {
  # create new data frame with possibly relevant columns
  keep <- c("game_date",
            "game_year",
            "game_month",
            "player_name",
            "batter",
            "bbref_id",  # I added bbref_id and lahman_id in 'pull pitch data from statcast.R'
            "lahman_id",
            "pitcher",
            "events",
            # "description",
            # "zone",
            "des",
            "game_type",
            "stand",
            "p_throws",
            "home_team",
            "away_team",
            # "type",
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
            # "sv_id",
            # "hit_distance_sc",
            "launch_speed",
            "launch_angle",
            # "effective_speed",
            # "game_pk",
            # "launch_speed_angle",
            "babip_value",  # use babip_value and iso_value to classify out, single, double, etc.
            "iso_value")
  batted <- df[keep]
  
  # only keep regular season games
  batted <- subset(batted,game_type=="R")
  
  # calculate spray angle
  # reference: https://www.fangraphs.com/tht/research-notebook-new-format-for-statcast-data-export-at-baseball-savant/
  batted$spray_angle <- with(batted, round(
    (atan(
      (hc_x-125.42)/(198.27-hc_y)
    )*180/pi*.75)
    ,1)
  )
  
  # remove any rows with NA values in key columns
  batted <- subset(batted,!is.na(launch_speed) & !is.na(launch_angle) & !is.na(spray_angle))
  
  # add speed scores pulled from FanGraphs
  batted <- add_speed_scores(batted)
  
  batted$babip_iso <- as.factor(with(batted,paste0(babip_value,"_",iso_value)))
  
  # classify events into out/single/double/triple/HR
  batted$class <- batted$babip_iso
  levels(batted$class)[levels(batted$class)=="0_0"] <- "out"
  levels(batted$class)[levels(batted$class)=="1_0"] <- "single"
  levels(batted$class)[levels(batted$class)=="1_1"] <- "double"
  levels(batted$class)[levels(batted$class)=="1_2"] <- "triple"
  levels(batted$class)[levels(batted$class)=="0_3"] <- "home_run"
  
  # add linear weight values
  # linear weights reference: https://www.fangraphs.com/library/principles/linear-weights/
  # values from https://www.fangraphs.com/guts.aspx?type=cn
  # currently using 2017 values
  batted$linear_weight <- lw["out"]
  batted$linear_weight[batted$class=="single"] <- lw["single"]
  batted$linear_weight[batted$class=="double"] <- lw["double"]
  batted$linear_weight[batted$class=="triple"] <- lw["triple"]
  batted$linear_weight[batted$class=="home_run"] <- lw["home_run"]
  batted$linear_weight <- round(batted$linear_weight,3)
  
  batted$events <- factor(batted$events)
  
  return(batted)
}

#' @param years vector of years (take average of coefficients across these years)
#' 
#' @return linear weight values and the multiplier to convert to OBP scale
#' 
set_linear_weights <- function(years=2017) {
  # add linear weight values
  # linear weights reference: https://www.fangraphs.com/library/principles/linear-weights/
  # values from https://www.fangraphs.com/guts.aspx?type=cn
  # currently using 2017 values
  lw.df <- read.csv("./linear weights by year.csv")
  lw.df <- subset(lw.df, Season %in% years)
  
  multiplier <- mean(lw.df$wOBAScale)
  # out,single,double,triple,home_run,walk,HBP
  lw <- with(lw.df, c(0, mean(w1B), mean(w2B), mean(w3B), mean(wHR), mean(wBB), mean(wHBP))/multiplier)
  names(lw) <- c("out","single","double","triple","home_run","walk","HBP")
  
  return(list(lw=lw, multiplier=multiplier))
}

#' 
#' @param probs an array of probabilities for out, single, double, etc.
#' @param lw vector of linear weights
#' 
#' @return vector of predicted linear weights
#' 
predict_lw_from_probs <- function(probs, lw) {
  classes <- c("out","single","double","triple","home_run")
  return(probs[,classes] %*% lw[classes])
}

#' 
#' @param prefix the prefix for the column names (e.g., multinom, rf, knn, etc.)
#' @param df data frame of batted balls to add prediction columns to
#' @param probs predicted probabilities of each class (single, double, etc.)
#' 
#' @return df with columns for predicted linear weight and probability of each hit type
#' 
add_preds_from_probs <- function(prefix, df, probs, lw) {
  # predict linear weights and add to df
  df[,paste0(prefix,"_linear_weight")] <- predict_lw_from_probs(probs, lw)
  
  # add probs for 1B, 2B, 3B, HR
  for (class in c("single","double","triple","home_run")) {
    df[,paste0(prefix,"_",class)] <- probs[,class]
  }
  
  return(df)
}

#' 
#' @param df data frame
#' @param k value of k to use
#' @param trainSize proportion of data to use for training the model
#' @param seed seed to set
#' 
#' @return a kNN regression model
#' 
fit_knn_regression_model <- function(df, k, trainSize, seed=NULL) {
  require(caret)
  
  if (!is.null(seed)) { set.seed(seed) }
  n <- dim(df)[1]
  which.train <- sample(1:n,floor(n*trainSize))
  training <- df[which.train,]
  
  fitCtrl <- trainControl(method = "none")
  knnmod <- train(linear_weight ~ ., data=training[c("linear_weight","launch_speed","launch_angle","spray_angle")],
                  method="knn", trControl=fitCtrl, tuneGrid=expand.grid(k=k),
                  preProcess=c("scale","center"))
  
  return(knnmod)
}

#' 
#' @param df data frame
#' @param k value of k to use
#' @param trainSize proportion of data to use for training the model
#' @param seed seed to set
#' 
#' @return a kNN classification model
#' 
fit_knn_classification_model <- function(df, k, trainSize, seed=NULL) {
  require(caret)
  
  if (!is.null(seed)) { set.seed(seed) }
  n <- dim(df)[1]
  which.train <- sample(1:n,floor(n*trainSize))
  training <- df[which.train,]
  
  fitCtrl <- trainControl(method = "none")
  knnmod <- train(class ~ ., data=training[c("class","launch_speed","launch_angle","spray_angle")],
                  method="knn", trControl=fitCtrl, tuneGrid=expand.grid(k=k),
                  preProcess=c("scale","center"))
  
  return(knnmod)
}

#' 
#' @param df data frame (batted)
#' @param lw.prefixes vector of prefixes for models with linear weight predictions
#' @param full.prefixes vector of prefixes for models with full predictions
#' @param by_month set to TRUE to group data by year and month
#' 
#' @return a data table grouped by player and year with linear weights and predicted linear weights
#' 
group_weights_by_year <- function(df, lw.prefixes=NULL, full.prefixes=NULL, by_month=FALSE) {
  require(data.table)
  
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
    weights.dt <- data.table(df[c("lahman_id","batter","game_year","game_month",
                                  "Spd","linear_weight",cols)])
    colnames(weights.dt)[colnames(weights.dt)=="batter"] <- "mlb_id"
    # .SD is a subset with all columns except the grouping columns
    weights.dt <- weights.dt[,lapply(.SD,sum),by=list(lahman_id,mlb_id,game_year,game_month,Spd)]
  }
  else {
    weights.dt <- data.table(df[c("lahman_id","batter","game_year"
                                  ,"Spd","linear_weight",cols)])
    colnames(weights.dt)[colnames(weights.dt)=="batter"] <- "mlb_id"
    # .SD is a subset with all columns except the grouping columns
    weights.dt <- weights.dt[,lapply(.SD,sum),by=list(lahman_id,mlb_id,game_year,Spd)]
  }
  
  return(weights.dt)
}


#' 
#' @param weights.dt data.table of weights from group_weights_by_year
#' @param lw.prefixes vector of prefixes for models with linear weight predictions
#' @param full.prefixes vector of prefixes for models with full predictions
#' 
#' @return a data table grouped by player and year with predicted linear weights and wOBA
#' 
add_preds_to_yearly_data <- function(weights.dt, lw.prefixes=NULL, full.prefixes=NULL) {
  require(Lahman)
  
  if (is.null(lw.prefixes)) { lw.prefixes <- get_prefixes(weights.dt, type="lw") }
  if (is.null(full.prefixes)) { full.prefixes <- get_prefixes(weights.dt, type="full") }
  
  minYear <- min(weights.dt$game_year)
  maxYear <- max(weights.dt$game_year)
  
  data("Batting")
  batting.dt <- data.table(Batting)
  batting.dt <- subset(batting.dt,yearID %in% minYear:maxYear)
  
  if (maxYear>=2017 & max(batting.dt$yearID)<=2017){
    load("./lahman.batting.2017.RData")
    batting.dt <- rbind(batting.dt,lahman.batting.2017)
  }
  
  # sum the relevant statistics for computing wOBA
  # group by playerID and yearID to account for players who changed teams
  batting.dt <- batting.dt[,list(AB=sum(AB),X1B=sum(H-X2B-X3B-HR),X2B=sum(X2B),X3B=sum(X3B),HR=sum(HR),
                                 BB=sum(BB),IBB=sum(IBB),HBP=sum(HBP),SF=sum(SF),SH=sum(SH),SB=sum(SB),
                                 SO=sum(SO),PA=sum(AB+BB+SF+SH+HBP)),
                           by=list(playerID,yearID)]
  
  # description of wOBA: https://www.fangraphs.com/library/offense/woba/
  # weights by year: https://www.fangraphs.com/guts.aspx?type=cn
  tmp <- set_linear_weights()
  lw <- tmp$lw
  lw_multiplier <- tmp$multiplier
  
  # calculate true wOBA from Lahman
  batting.dt$wOBA <- with(batting.dt, (lw_multiplier * (lw["walk"]*(BB-IBB) + lw["HBP"]*HBP + 
                                                          lw["single"]*X1B + lw["double"]*X2B + 
                                                          lw["triple"]*X3B + lw["home_run"]*HR) / PA))
  
  # merge batting.dt and weights.dt
  colnames(weights.dt)[colnames(weights.dt)=="lahman_id"] <- "playerID"
  colnames(weights.dt)[colnames(weights.dt)=="game_year"] <- "yearID"
  colnames(weights_by_month.dt)[colnames(weights_by_month.dt)=="lahman_id"] <- "playerID"
  colnames(weights_by_month.dt)[colnames(weights_by_month.dt)=="game_year"] <- "yearID"
  batting.dt <- merge(batting.dt,weights.dt,by=c("playerID","yearID"),all=TRUE)
  
  # add predicted wOBA values from models
  # use true values for AB, BB, etc.
  for (pre in lw.prefixes) {
    batting.dt[[paste0(pre,"_wOBA")]] <- with(batting.dt, (lw_multiplier*(batting.dt[[paste0(pre,"_linear_weight")]] + 
                                                                          lw["walk"]*(BB-IBB) + lw["HBP"]*HBP) / PA))
  }
  
  return(batting.dt)
}

#' 
#' @param batting.dt data table from add_preds_to_yearly_data
#' 
#' @return lagged data table with year and previous year values in each row
#' 
lag_yearly_data <- function(batting.dt) {
  batting_lagged <- batting.dt
  batting_lagged$next_year <- batting_lagged$yearID + 1
  batting_lagged <- merge(batting_lagged[,!"next_year"],batting_lagged[,!"yearID"],
                          by.x=c("playerID","yearID"),by.y=c("playerID","next_year"))
  colnames(batting_lagged) <- gsub(".x","",colnames(batting_lagged),fixed=TRUE)
  colnames(batting_lagged) <- gsub(".y",".prev",colnames(batting_lagged),fixed=TRUE)
  
  return(batting_lagged)
}

#' 
#' @param data data frame with data to plot
#' @param x.col name of column in data to use for x axis
#' @param y.col name of column in data to use for y axis
#' @param xlab x-axis label (default: x.col)
#' @param ylab y-axis label (default: y.col)
#' @param plotTitle title for plot (default: ylab vs. xlab)
#' @param includeCorrelation if TRUE, show correlation of x,y on second line of title
#' @param include_y_equal_x if TRUE, show y=x line
#' 
#' @return a ggplot object
#' 
basic_scatterplot <- function(data,x.col,y.col,xlab=substitute(x.col),
                              ylab=substitute(y.col),plotTitle=paste0(ylab," vs. ",xlab),
                              includeCorrelation=TRUE,include_y_equal_x=TRUE) {
  require(ggplot2)
  
  x <- unname(unlist(data[,x.col,with=FALSE]))
  y <- unname(unlist(data[,y.col,with=FALSE]))
  if (includeCorrelation == TRUE) { plotTitle <- paste0(plotTitle,"\nCorrelation: ",round(cor(x,y),3)) }
  
  p <- (ggplot(data=data,aes(x=x,y=y,text=paste0(playerID,"_",yearID)))
           + geom_point(alpha=.75)
           + labs(x=xlab,y=ylab,title=plotTitle)
  )
  
  if (include_y_equal_x == TRUE) {
    p <- p + geom_abline(slope=1, intercept=0, color='red', lty=2)
  }
  
  return(p)
}

#' 
#' @param data data frame with data to plot
#' @param x.col name of column in data to use for x axis
#' @param y.col name of column in data to use for y axis
#' @param color.col name of column in data to use for point color
#' @param xlab x-axis label (default: x.col)
#' @param ylab y-axis label (default: y.col)
#' @param plotTitle title for plot (default: ylab vs. xlab)
#' @param includeCorrelation if TRUE, show correlation of x,y on second line of title
#' @param include_y_equal_x if TRUE, show y=x line
#' @param colorBarTitle title for color bar
#' 
#' @return a ggplot object
#' 
color_scatterplot <- function(data,x.col,y.col,color.col=NULL,xlab=substitute(x.col),
                              ylab=substitute(y.col),plotTitle=paste0(ylab," vs. ",xlab),
                              includeCorrelation=TRUE,include_y_equal_x=TRUE,
                              colorBarTitle=color.col) {
  require(ggplot2)
  
  if (is.null(color.col)) {
    # if no color, call basic_scatterplot instead
    return(basic_scatterplot(data,x.col,y.col,xlab,ylab,plotTitle,includeCorrelation,include_y_equal_x))
  }
  else{
    x <- unname(unlist(data[,x.col,with=FALSE]))
    y <- unname(unlist(data[,y.col,with=FALSE]))
    if (!is.null(color.col)) { color <- unname(unlist(data[,color.col,with=FALSE])) }
    if (includeCorrelation == TRUE) { plotTitle <- paste0(plotTitle,"\nCorrelation: ",round(cor(x,y),3)) }
    
    p <- (ggplot(data=data,aes(x=x,y=y,color=color,text=paste0(playerID,"_",yearID)))
          + geom_point(alpha=.75)
          + labs(x=xlab,y=ylab,title=plotTitle)
          + scale_colour_gradientn(colours = terrain.colors(5),
                                   guide=guide_colorbar(title=colorBarTitle))
    )
    
    if (include_y_equal_x == TRUE) {
      p <- p + geom_abline(slope=1, intercept=0, color='red', lty=2)
    }
    
    return(p)
  }
}

#' any columns containing "_linear_weight" have linear weight predictions for that model
#' any columns containing "_home_run" have full predictions for that model
#' 
#' @param batted balls data frame
#' @param type "lw" for linear weight predictions, "full" for full predictions
#' 
#' @return a vector of model prefixes
#' 
get_prefixes <- function(df, type="none") {
  if (type != "lw" & type != "full") {
    print("Must set type to 'lw' or 'full'.")
    return(NA)
  }
  col_names <- colnames(df)
  if (type=="lw") { indices <- grep("_linear_weight", col_names) }
  else { indices <- grep("_home_run", col_names) }
  full <- col_names[indices]
  prefix <- sapply(strsplit(full,"_"),'[',1)
  return(prefix)
}

#' speed scores available from FanGraphs:
#' https://www.fangraphs.com/library/offense/spd/ 
#' 
#' @param batted data frame of batted balls
add_speed_scores <- function(batted) {
  require(data.table)
  speed_scores <- read.csv("./speed scores.csv")
  speed_scores <- data.table(speed_scores)
  
  # group by MLB ID and year in case there are multiple entries for a player per year (not sure if this happens)
  # (should really do a weighted average of the player's two scores, but this is good enough)
  speed_scores <- speed_scores[,list(Spd=mean(Spd)),by=list(mlb_id,year)]
  
  batted <- merge(batted, speed_scores, by.x=c("batter","game_year"), by.y=c("mlb_id","year"),
                  all.x=TRUE)
  
  return(batted)
}

#' get a player's most common position for the years in df
#' 
#' @param df data frame to add position to
#' @param id_col name of column in df with Lahman ID
#' @param year_col name of column in df with year
#' 
#' @return df with POS column for most commonly played position for each player
#' 
add_positions <- function(df, id_col="playerID", year_col="yearID") {
  require(Lahman)
  require(data.table)
  df <- data.frame(df)  # in case it's a data.table object (syntax would be different)
  years <- unique(df[,year_col])
  data(Fielding)
  Fielding.subset <- data.table(subset(Fielding, yearID %in% years))
  if (max(years)>=2017 & max(Fielding$yearID)<2017) {
    # R package hasn't been updated w/ 2017 yet
    load("./lahman.fielding.2017.RData")
    Fielding.subset <- rbind(Fielding.subset, lahman.fielding.2017)
  }
  
  grouped <- Fielding.subset[,list(G=sum(G)),by=list(playerID,POS)]
  grouped.agg <- aggregate(G ~ playerID, grouped, max)
  grouped <- merge(grouped, grouped.agg)
  
  # if there's a tie, just pick the first line
  grouped <- grouped[!duplicated(grouped$playerID)]
  
  # left join df with grouped fielding table
  df <- merge(x=df, y=grouped[,c("playerID","POS")], by.x=id_col, by.y="playerID", all.x=TRUE)
  return(df)
}


marcel_projections <- function(year, pred_df=NULL, model_prefix=NULL, lw_years=year,
                               pred_df_year_col="yearID", pred_df_id_col="playerID",
                               verbose=FALSE) {
  require(Lahman)
  require(data.table)
  
  if (!is.null(pred_df) & is.null(model_prefix)) {
    print("Must specify which model's predictions to use in 'model' parameter.")
    return(NULL)
  }
  
  # get league average data by year for past three years
  past.years <- c(year-3,year-2,year-1)
  data(Batting)
  batting.sub <- subset(Batting, yearID %in% past.years)
  if (max(past.years)>=2017 & max(batting.sub$yearID)<2017) {
    # R package hasn't been updated w/ 2017 yet
    load("./lahman.batting.2017.RData")
    batting.sub <- rbind(batting.sub, lahman.batting.2017)
  }
  batting.sub <- add_positions(batting.sub)
  batting.sub <- subset(batting.sub, POS!="P")  # exclude pitchers from league average hitting stats
  batting.sub <- data.table(batting.sub)
  batting.sub <- batting.sub[,list(AB=sum(AB),H=sum(H),X1B=sum(H-X2B-X3B-HR),X2B=sum(X2B),X3B=sum(X3B),HR=sum(HR),
                                   BB=sum(BB),IBB=sum(IBB),HBP=sum(HBP),SF=sum(SF),SH=sum(SH),SB=sum(SB),
                                   SO=sum(SO),PA=sum(AB+BB+SF+SH+HBP))
                             ,by=list(playerID,yearID)]  # combined stats for players who changed teams
  
  ### decision point: should league averages be computed from actual stats, or expected  ###
  ###                 stats from models? currently using actual stats                    ###
  sdcols <- colnames(batting.sub)[!(colnames(batting.sub) %in% c("playerID","yearID"))]
  league.avg <- batting.sub[,lapply(.SD,sum), by=list(yearID), .SDcols=sdcols]
  league.avg <- data.frame(setorder(league.avg, -yearID))  # year-1, then year-2, then year-3
  
  # for easy lookup:
  batting.sub <- data.frame(batting.sub)
  rownames(batting.sub) <- with(batting.sub, paste0(playerID,"_",yearID))
  
  # create projections for all players (non-pitchers) with batting data in previous season
  marcel.df <- data.frame(yearID=year, playerID=subset(batting.sub, yearID==year-1)$playerID)
  marcel.df <- add_player_age(marcel.df)
  
  if (is.null(pred_df)) {
    # only use Lahman data
    prev1 <- subset(batting.sub, yearID==year-1)
    prev2 <- subset(batting.sub, yearID==year-2)
    prev3 <- subset(batting.sub, yearID==year-3)
  }
  else {
    pred_df <- data.frame(pred_df)
    colnames(pred_df)[colnames(pred_df)==pred_df_year_col] <- "yearID"
    colnames(pred_df)[colnames(pred_df)==pred_df_id_col] <- "playerID"
    
    # remove pitchers
    pred_df <- add_positions(pred_df)
    pred_df <- subset(pred_df, POS!="P")
    
    # do some formatting so we have the same columns as batting.sub for later operations
    cols <- c("playerID","yearID",paste0(model_prefix,"_",c("single","double","triple","home_run")),
              "PA","BB","IBB","HBP","SF","SH","SO","SB")
    pred_df <- pred_df[,cols]
    colnames(pred_df)[colnames(pred_df)==paste0(model_prefix,"_single")] <- "X1B"
    colnames(pred_df)[colnames(pred_df)==paste0(model_prefix,"_double")] <- "X2B"
    colnames(pred_df)[colnames(pred_df)==paste0(model_prefix,"_triple")] <- "X3B"
    colnames(pred_df)[colnames(pred_df)==paste0(model_prefix,"_home_run")] <- "HR"
    rownames(pred_df) <- with(pred_df, paste0(playerID,"_",yearID))
    
    # (tried doing this with ifelse and it didn't work)
    pred.years <- unique(pred_df$yearID)
    if ((year-1) %in% pred.years) { prev1 <- subset(pred_df, yearID==year-1) }
    else { prev1 <- subset(batting.sub, yearID==year-1) }
    if ((year-2) %in% pred.years) { prev2 <- subset(pred_df, yearID==year-2) }
    else { prev2 <- subset(batting.sub, yearID==year-2) }
    if ((year-3) %in% pred.years) { prev3 <- subset(pred_df, yearID==year-3) }
    else { prev3 <- subset(batting.sub, yearID==year-3) }
    
    if (verbose==TRUE){
      for (i in 1:3) {
        if (!((year-i) %in% pred.years)) { print(paste0(year-i," from Lahman")) }
      }
    }
  }
  
  # Expected PA: (.5*prev1) + (.1*prev2) + 200
  # If data is missing (player didn't play that year), count as 0
  PA1 <- ifelse(!is.na(prev1[paste0(marcel.df$playerID,"_",year-1),"PA"]),
                prev1[paste0(marcel.df$playerID,"_",year-1),"PA"],
                0)
  PA2 <- ifelse(!is.na(prev2[paste0(marcel.df$playerID,"_",year-2),"PA"]),
                prev2[paste0(marcel.df$playerID,"_",year-2),"PA"],
                0)
  PA3 <- ifelse(!is.na(prev3[paste0(marcel.df$playerID,"_",year-3),"PA"]),
                prev3[paste0(marcel.df$playerID,"_",year-3),"PA"],
                0)
  marcel.df$xPA <- 0.5*PA1 + 0.1*PA2 + 200
  
  # Expected values for other stats:
  #  - weight value from previous 3 seasons as 5/4/3 (:= weighted.val)
  #  - weight PAs from previous 3 seasons as 5/4/3 (:= weighted.PAs)
  #  - prorate league average stats to player's number of PAs, then weight as 5/4/3
  #  - calculate player's league average rate as weighted value / # of PAs
  #  - prorate this leave average to 1200 ABs (:= weighted.avg)
  #  - calculate projected rate: (weighted.avg + weighted.val) / (1200 + weighted.PAs)
  #  - multiply projected rate by projected PAs
  #  - age adjustment: multiply projected values by 1 + [(29 - age) * .006]
  
  weighted.PAs <- 5*PA1 + 4*PA2 + 3*PA3
  age.adjust <- (29 - marcel.df$age) * 0.006
  for (val in c("X1B","X2B","X3B","HR","BB","IBB","HBP","SF","SH")) {
    val1 <- ifelse(!is.na(prev1[paste0(marcel.df$playerID,"_",year-1),val]),
                   prev1[paste0(marcel.df$playerID,"_",year-1),val],
                   0)
    val2 <- ifelse(!is.na(prev2[paste0(marcel.df$playerID,"_",year-2),val]),
                   prev2[paste0(marcel.df$playerID,"_",year-2),val],
                   0)
    val3 <- ifelse(!is.na(prev3[paste0(marcel.df$playerID,"_",year-3),val]),
                   prev3[paste0(marcel.df$playerID,"_",year-3),val],
                   0)
    weighted.val <- 5*val1 + 4*val2 + 3*val3
    weighted.avg <- (5*PA1*league.avg[1,val]/league.avg[1,"PA"] 
                     + 4*PA2*league.avg[2,val]/league.avg[2,"PA"]
                     + 3*PA3*league.avg[3,val]/league.avg[3,"PA"])
    weighted.avg <- weighted.avg/weighted.PAs * 1200
    rate <- (weighted.avg + weighted.val) / (1200 + weighted.PAs)
    
    marcel.df[,paste0("x",val)] <- rate * marcel.df$xPA * (1 + age.adjust)
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
  return(marcel.df)
}


add_player_age <- function(df, id_col="playerID", year_col="yearID") {
  require(Lahman)
  data(Batting)
  data(Master)
  
  cols <- c("playerID","birthYear","birthMonth","birthDay")
  birthdate.df <- Master[cols]
  df <- data.frame(df)
  if (max(df[,year_col])>=2017 & max(Batting$yearID)<2017) {
    # R package hasn't been updated w/ 2017 yet
    load("./lahman.master.2017.RData")
    birthdate.df <- rbind(birthdate.df, lahman.master.2017[cols])
  }
  birthdate.df <- unique(birthdate.df)
  
  tmp <- merge(df[,c(id_col,year_col)], birthdate.df, by.x=id_col, by.y="playerID", all.x=TRUE)
  tmp$age <- ifelse(tmp$birthMonth<7, tmp[,year_col]-tmp$birthYear, tmp[,year_col]-tmp$birthYear-1)
  
  return(merge(df, tmp[c(id_col,year_col,"age")], by=c(id_col,year_col), all.x=TRUE))
}


get_eval_df <- function(year, lw_years=year, pred_df, prefixes=get_prefixes(pred_df,type='full')) {
  require(Lahman)
  data(Batting)
  
  # 'vals' will be true stats for that year
  vals <- Batting
  if (year>=2017 & max(Batting$yearID<2017)) {
    load("./lahman.batting.2017.RData")
    vals <- rbind(Batting, lahman.batting.2017)
  }
  vals <- subset(vals, yearID==year)
  vals$PA  <- with(vals, AB+BB+SF+SH+HBP)
  vals$X1B <- with(vals, H-X2B-X3B-HR)
  vals$BA  <- with(vals, H/AB)
  vals$OBP <- with(vals, (H+BB+HBP)/PA)
  vals$SLG <- with(vals, (X1B + 2*X2B + 3*X3B + 4*HR)/AB)
  vals$OPS <- with(vals, OBP+SLG)
  tmp <- set_linear_weights(lw_years)
  lw <- tmp$lw
  lw_multiplier <- tmp$multiplier
  vals$wOBA <- with(vals, (lw_multiplier * (lw["walk"]*(BB-IBB) + lw["HBP"]*HBP + 
                                              lw["single"]*X1B + lw["double"]*X2B + 
                                              lw["triple"]*X3B + lw["home_run"]*HR) / PA))
  vals <- vals[c("playerID","yearID","")]
  
  marcel <- marcel_projections(year, lw_years=lw_years)
  df <- merge(x=marcel, y=vals, by)
}

