# I don't think this one is actually used...
daily_batter_bref_with_id <- function(t1, t2) {
  require(dplyr)
  require(rvest)
  require(xml2)
  
  df <- read_html(paste0("http://www.baseball-reference.com/leagues/daily.cgi?user_team=&bust_cache=&type=b&lastndays=7&dates=fromandto&fromandto=", 
                         t1, ".", t2, "&level=mlb&franch=&stat=&stat_value=0"))
  mlb_ids <- html_attr(html_nodes(df,"a"),"href")
  mlb_ids <- mlb_ids[grepl("redirect.fcgi",mlb_ids)]
  mlb_ids <- as.numeric(sapply(strsplit(mlb_ids,"mlb_ID="),`[`,2))
  df <- df %>% html_nodes(xpath = "//*[@id=\"daily\"]") %>% 
    html_table(fill = TRUE)
  df <- as.data.frame(df)[-c(1, 3, 5)]
  names(df)[1:4] <- c("Name", "Age", "Level", "Team")
  df[, c(2, 5:26)] <- lapply(df[, c(2, 5:26)], as.numeric)
  df$X1B <- with(df, H - (X2B + X3B + HR))
  season <- substr(t1, 1, 4)
  df$season <- season
  df$uBB <- with(df, BB - IBB)
  df <- df[, c(28, 1:9, 27, 10:15, 29, 16:26)]
  df$Team <- gsub(" $", "", df$Team, perl = T)
  df <- filter_(df, ~Name != "Name")
  df$mlb_id <- mlb_ids
  df <- arrange_(df, ~desc(PA), ~desc(OPS))
  df
}

#'
#' @param df data frame (should be created from CSV file)
#' @param lw vector of linear weights
#' 
#' @return a data frame with all of the necessary columns
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
            "description",
            "zone",
            "des",
            "game_type",
            "stand",
            "p_throws",
            "home_team",
            "away_team",
            "type",
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
            "sv_id",
            "hit_distance_sc",
            "launch_speed",
            "launch_angle",
            "effective_speed",
            "game_pk",
            "launch_speed_angle",
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

#' @return linear weight values and the multiplier to convert to OBP scale
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
predict_lw_from_probs <- function(probs, lw) {
  classes <- c("out","single","double","triple","home_run")
  return(probs[,classes] %*% lw[classes])
}

#' 
#' @param df data frame
#' @param k value of k to use
#' @param trainSize proportion of data to use for training the model
#' @param seed seed to set
#' 
#' @return a kNN regression model
#' 
fit_knn_regression_model <- function(df, k, trainSize, seed=1) {
  require(caret)
  
  set.seed(seed)
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
#' @param df data frame (batted)
#' @param lwCols vector of column names with linear weight predictions
#' 
#' @return a data table grouped by player and year with linear weights and predicted linear weights
#' 
group_weights_by_year <- function(df, lwCols) {
  require(data.table)
  weights.dt <- data.table(df[c("lahman_id","batter","game_year","linear_weight",lwCols)])
  colnames(weights.dt)[colnames(weights.dt)=="batter"] <- "mlb_id"
  
  # .SD is a subset with all columns except the grouping columns
  weights.dt <- weights.dt[,lapply(.SD,sum),by=list(lahman_id,mlb_id,game_year)]
  
  return(weights.dt)
}

#' 
#' @param df data frame (batted)
#' @param lwCols vector of column names with linear weight predictions
#' 
#' @return a data table grouped by player, year, and month with linear weights and predicted linear weights
#' 
group_weights_by_year_month <- function(df, lwCols) {
  require(data.table)
  weights_by_month.dt <- data.table(df[c("lahman_id","batter","game_year",
                                         "game_month","linear_weight",lwCols)])
  colnames(weights_by_month.dt)[colnames(weights_by_month.dt)=="batter"] <- "mlb_id"
  
  # .SD is a subset with all columns except the grouping columns
  weights_by_month.dt <- weights_by_month.dt[,lapply(.SD,sum),
                                             by=list(lahman_id,mlb_id,game_year,game_month)]
  
  return(weights_by_month.dt)
}

#' 
#' @param weights.dt data.table of weights from group_weights_by_year
#' @param lwCols vector of column names with linear weight predictions
#' 
#' @return a data table grouped by player and year with predicted linear weights and wOBA
#' 
add_wOBA_preds_to_yearly_data <- function(weights.dt, lwCols) {
  require(Lahman)
  
  minYear <- min(weights.dt$game_year)
  maxYear <- max(weights.dt$game_year)
  
  data("Batting")
  batting.dt <- data.table(Batting)
  batting.dt <- subset(batting.dt,yearID %in% minYear:maxYear)
  
  if (maxYear==2017 & max(batting.dt$yearID)!=2017){
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
  
  # calculate true wOBA
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
  wOBA.cols <- gsub("linear_weight","wOBA",lwCols)
  for (i in 1:length(lwCols)) {
    batting.dt[[wOBA.cols[i]]] <- with(batting.dt, (lw_multiplier*(batting.dt[[lwCols[i]]] + 
                                                                   lw["walk"]*(BB-IBB) + lw["HBP"]*HBP) / 
                                                    (AB + BB - IBB + SF + HBP)))
  }
  # batting.dt$lm_wOBA <- with(batting.dt,
  #                            lw_multiplier*(lm_linear_weight + lw["walk"]*(BB-IBB) + lw["HBP"]*HBP) / (AB + BB - IBB + SF + HBP))
  # batting.dt$rf_wOBA_1 <- with(batting.dt,
  #                              lw_multiplier*(rf_linear_weight + lw["walk"]*(BB-IBB) + lw["HBP"]*HBP) / (AB + BB - IBB + SF + HBP))
  # batting.dt$rf_wOBA_2 <- with(batting.dt,
  #                              lw_multiplier*(rf_linear_weight_2 + lw["walk"]*(BB-IBB) + lw["HBP"]*HBP) / (AB + BB - IBB + SF + HBP))
  
  return(batting.dt)
}

#' 
#' @param batting.dt data table from add_wOBA_preds_to_yearly_data
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


basic_scatterplot <- function(data,x.col,y.col,xlab=substitute(x.col),
                              ylab=substitute(y.col),plotTitle=paste0(ylab," vs. ",xlab),
                              includeCorrelation=TRUE) {
  require(ggplot2)
  
  x <- unname(unlist(data[,x.col,with=FALSE]))
  y <- unname(unlist(data[,y.col,with=FALSE]))
  if (includeCorrelation == TRUE) { plotTitle <- paste0(plotTitle,"\nCorrelation: ",round(cor(x,y),3)) }
  
  p <- (ggplot(data=data,aes(x=x,y=y,text=paste0(playerID,"_",yearID)))
           + geom_point(alpha=.75)
           + labs(x=xlab,y=ylab,title=plotTitle)
  )
  
  return(p)
}

color_scatterplot <- function(data,x.col,y.col,color.col=NULL,xlab=substitute(x.col),
                              ylab=substitute(y.col),plotTitle=paste0(ylab," vs. ",xlab),
                              includeCorrelation=TRUE,colorBarTitle=color.col) {
  require(ggplot2)
  
  if (is.null(color.col)) {
    return(basic_scatterplot(data,x.col,y.col,xlab,ylab,plotTitle,includeCorrelation))
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
    
    return(p)
  }
}
  
  
  
  
  
