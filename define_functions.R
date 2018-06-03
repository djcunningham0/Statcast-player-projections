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

#' 
#' @return linear weight values and the multiplier to convert to OBP scale
#' 
set_linear_weights <- function() {
  # add linear weight values
  # linear weights reference: https://www.fangraphs.com/library/principles/linear-weights/
  # values from https://www.fangraphs.com/guts.aspx?type=cn
  # currently using 2017 values
  multiplier <- 1.185
  lw <- c(0,0.877,1.232,1.552,1.980,0.693,0.723)/multiplier  # out,single,double,triple,home_run,walk,HBP
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
#' @param lwCols vector of prefixes for models with linear weight predictions
#' @param fullCols vector of prefixes for models with full predictions
#' @param by_month set to TRUE to group data by year and month
#' 
#' @return a data table grouped by player and year with linear weights and predicted linear weights
#' 
group_weights_by_year <- function(df, lwCols, fullCols, by_month=FALSE) {
  require(data.table)
  cols <- c()
  for (pre in lwCols) {
    cols <- c(cols, paste0(pre,"_linear_weight"))
    if (pre %in% fullCols) {
      cols <- c(cols, paste0(pre,"_",c("single","double","triple","home_run")))
    }
  }
  
  if (by_month==TRUE) {
    weights.dt <- data.table(df[c("lahman_id","batter","game_year","game_month","linear_weight",cols)])
    colnames(weights.dt)[colnames(weights.dt)=="batter"] <- "mlb_id"
    # .SD is a subset with all columns except the grouping columns
    weights.dt <- weights.dt[,lapply(.SD,sum),by=list(lahman_id,mlb_id,game_year,game_month)]
  }
  else {
    weights.dt <- data.table(df[c("lahman_id","batter","game_year","linear_weight",cols)])
    colnames(weights.dt)[colnames(weights.dt)=="batter"] <- "mlb_id"
    # .SD is a subset with all columns except the grouping columns
    weights.dt <- weights.dt[,lapply(.SD,sum),by=list(lahman_id,mlb_id,game_year)]
  }
  
  return(weights.dt)
}


#' 
#' @param weights.dt data.table of weights from group_weights_by_year
#' @param lwCols vector of prefixes for models with linear weight predictions
#' @param fullCols vector of prefixes for models with full predictions
#' 
#' @return a data table grouped by player and year with predicted linear weights and wOBA
#' 
add_preds_to_yearly_data <- function(weights.dt, lwCols, fullCols) {
  require(Lahman)
  
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
  batting.dt <- batting.dt[,list(AB=sum(AB),X1B=sum(H-X2B-X3B-HR),X2B=sum(X2B),X3B=sum(X3B),
                                 HR=sum(HR),BB=sum(BB),IBB=sum(IBB),HBP=sum(HBP),SF=sum(SF),SB=sum(SB)),
                           by=list(playerID,yearID)]
  batting.dt$SBperAB <- batting.dt$SB / batting.dt$AB
  
  # description of wOBA: https://www.fangraphs.com/library/offense/woba/
  # weights by year: https://www.fangraphs.com/guts.aspx?type=cn
  tmp <- set_linear_weights()
  lw <- tmp$lw
  lw_multiplier <- tmp$multiplier
  
  # calculate true wOBA from Lahman
  batting.dt$wOBA <- with(batting.dt, (lw_multiplier * (lw["walk"]*(BB-IBB) + lw["HBP"]*HBP + 
                                                          lw["single"]*X1B + lw["double"]*X2B + 
                                                          lw["triple"]*X3B + lw["home_run"]*HR) / 
                                         (AB + BB - IBB + SF + HBP)))
  
  # merge batting.dt and weights.dt
  colnames(weights.dt)[colnames(weights.dt)=="lahman_id"] <- "playerID"
  colnames(weights.dt)[colnames(weights.dt)=="game_year"] <- "yearID"
  colnames(weights_by_month.dt)[colnames(weights_by_month.dt)=="lahman_id"] <- "playerID"
  colnames(weights_by_month.dt)[colnames(weights_by_month.dt)=="game_year"] <- "yearID"
  batting.dt <- merge(batting.dt,weights.dt,by=c("playerID","yearID"),all=TRUE)
  
  # add predicted wOBA values from models
  # use true values for AB, BB, etc.
  for (pre in lwCols) {
    batting.dt[[paste0(pre,"_wOBA")]] <- with(batting.dt, (lw_multiplier*(batting.dt[[paste0(pre,"_linear_weight")]] + 
                                                                          lw["walk"]*(BB-IBB) + lw["HBP"]*HBP) / 
                                                           (AB + BB - IBB + SF + HBP)))
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
  
#' any columns containing "_home_run" have full predictions for that model
#' 
#' @param col_names the column names of the batted balls data frame containing model predictions
#' 
get_full_prediction_prefixes <- function(col_names) {
  indices <- grep("_home_run", col_names)
  full <- col_names[indices]
  prefix <- sapply(strsplit(full,"_"),'[',1)
  return(prefix)
}

#' any columns containing "_linear_weight" have linear weight predictions for that model
#' 
#' @param col_names the column names of the batted balls data frame containing model predictions
#' 
get_lw_prediction_prefixes <- function(col_names) {
  indices <- grep("_linear_weight", col_names)
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
  
  
  
