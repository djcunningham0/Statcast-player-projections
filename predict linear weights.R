# functions ---------------------------------------------------------------

source("./define_functions.R")


# prepare data frames -----------------------------------------------------

# batted_balls_2015-2017.csv comes from 'pull pitch data from statcast.R'
if (!exists("original_from_csv") || !is.data.frame(get("original_from_csv"))) {
  tryCatch(original_from_csv <- read.csv("batted_balls_2015-2017.csv"),
           error=function(err) {
             print("Missing Statcast data file.")
           })
}

tmp <- set_linear_weights()
lw <- tmp$lw
lw_multiplier <- tmp$multiplier
batted <- format_data_frame(original_from_csv,lw)


# fit linear model --------------------------------------------------------
lmod <- lm(linear_weight ~ launch_speed + launch_angle + spray_angle, data=batted)
batted$lm_linear_weight <- predict(lmod,newdata=batted)



# fit multinomial logistic regression model ----------------------------------------

library(nnet)
mod.multinom <- multinom(class ~ launch_speed + launch_angle + spray_angle, data=batted)
probs <- predict(mod.multinom,newdata=batted,type="prob")
batted <- add_preds_from_probs("multinom", batted, probs, lw)

# mod.multinom.stand <- multinom(class ~ launch_speed + launch_angle + spray_angle + stand, data=batted)
# probs.stand <- predict(mod.multinom.stand,newdata=batted,type="prob")
# batted$multinom_linear_weight_2 <- predict_lw_from_probs(probs.stand,lw)
# 
# mod.multinom.home <- multinom(class ~ launch_speed + launch_angle + spray_angle + home_team, data=batted)
# probs.home <- predict(mod.multinom.home,newdata=batted,type="prob")
# batted$multinom_linear_weight_3 <- predict_lw_from_probs(probs.home,lw)
# 
# mod.multinom.both <- multinom(class ~ launch_speed + launch_angle + spray_angle + stand + home_team, data=batted)
# probs.both <- predict(mod.multinom.both,newdata=batted,type="prob")
# batted$multinom_linear_weight_4 <- predict_lw_from_probs(probs.both,lw)



# fit LDA model -----------------------------------------------------------

library(MASS)
mod.lda <- lda(class ~ launch_speed + launch_angle + spray_angle, data=batted)
probs.lda <- predict(mod.lda,newdata=batted)$posterior
batted <- add_preds_from_probs("lda", batted, probs.lda, lw)


# fit random forest models ------------------------------------------------

### Need to fit new models with all of the data ###
library(randomForest)
load("./rf_models_032518.RData")

# set.seed(1)
# print(1)
# rf <- randomForest(linear_weight ~ launch_speed + launch_angle + spray_angle, data=batted)
# print("done")
# set.seed(1)
# print(2)
# rf.stand <- randomForest(linear_weight ~ launch_speed + launch_angle + spray_angle + stand, data=batted)
# print("done")
# set.seed(1)
# print(3)
# rf.home <- randomForest(linear_weight ~ launch_speed + launch_angle + spray_angle + home_team, data=batted)
# print("done")
# set.seed(1)
# print(4)
# rf.both <- randomForest(linear_weight ~ launch_speed + launch_angle + spray_angle + stand + home_team, data=batted)
# print(done)
# rf2017.class <- randomForest(class ~ launch_speed + launch_angle + spray_angle, data=batted2017)
# rf2017.class.stand <- randomForest(class ~ launch_speed + launch_angle + spray_angle + stand, data=batted2017)
# rf2017.class.home <- randomForest(class ~ launch_speed + launch_angle + spray_angle + home_team, data=batted2017)

# predict linear weight from classification model by taking dot prodict with linear weights vector
probs.rf <- predict(rf2017.class,newdata=batted,type="prob")
batted <- add_preds_from_probs("rf", batted, probs.rf, lw)

# predict linear weight from regression model
batted$rf2_linear_weight <- predict(rf2017,newdata=batted)




# fit kNN model -----------------------------------------------------------

library(caret)

# tune k with 'knn cross validation.R'
# k=50 works well

# currently fitting model on half the data
# could fit on all data, but would need to re-tune k and it won't make much difference
knnmod.class <- fit_knn_classification_model(batted, k=50, trainSize=0.5, seed=1)
probs.knn <- predict(knnmod.class,newdata=batted,type="prob")
probs.knn <- as.matrix(probs.knn)  # it was returning a list, which caused problems with the next function
batted <- add_preds_from_probs("knn", batted, probs.knn, lw)

# knnmod.reg <- fit_knn_regression_model(batted, k=50, trainSize=0.5, seed=1)
# batted$knn2_linear_weight <- predict(knnmod, newdata=batted)

# fit other models --------------------------------------------------------

# Ideas:
# use methods in caret package: http://topepo.github.io/caret/train-models-by-tag.html#Tree_Based_Model 
# see this website for info and examples for a lot of these: https://www.analyticsvidhya.com/blog/2017/09/common-machine-learning-algorithms/
# - Naive Bayes (probabilities for each class)
# - SVM?
# - XGBoost (boosted trees. use method="xgbTree" or "xgbLinear")
# - somehow account for the fact that the RF model underestimates wOBA for faster players
# - add age into the model, or adjust for future year's prediction
# - mixed effect vesion of multinomial model
# 
# add speed scores to all models

# group linear weights by player ------------------------------------------

# lw_cols stores names of columns in 'batted' with linear weight predictions
lw.prefixes <- get_lw_prediction_prefixes(colnames(batted))
full.prefixes <- get_full_prediction_prefixes(colnames(batted))

library(data.table)
weights.dt <- group_weights_by_year(batted, lw.prefixes, full.prefixes)
weights_by_month.dt <- group_weights_by_year(batted, lw.prefixes, full.prefixes, by_month=TRUE)


# add wOBA to Lahman database ---------------------------------------------

batting.dt <- add_preds_to_yearly_data(weights.dt, lw.prefixes, full.prefixes)
# need to do the same for monthly data if I'm going to do month-to-month correlations

batting_lagged <- lag_yearly_data(batting.dt)


# plots -------------------------------------------------------------------

library(ggplot2)
library(plotly)

AB_cutoff <- 100
plot_subset <- subset(batting_lagged,AB>=AB_cutoff & AB.prev>=AB_cutoff)

p.lm <- basic_scatterplot(data=subset(batting.dt,AB>=AB_cutoff),x.col="lm_wOBA",y.col="wOBA",
                          xlab="linear model wOBA prediction",plotTitle="wOBA vs. linear model prediction")
print(p.lm)
# ggplotly(p.lm)

# first few plots are just a gut check
# (make sure the models are correlated with wOBA)
p1 <- basic_scatterplot(data=subset(batting.dt,AB>=AB_cutoff),x.col="rf_wOBA",y.col="wOBA",
                        xlab="random forest wOBA prediction",plotTitle="wOBA vs. RF prediction")
print(p1)
# ggplotly(p1)

p3 <- basic_scatterplot(data=subset(batting.dt,AB>=AB_cutoff),x.col="knn_wOBA",y.col="wOBA",
                        xlab="kNN wOBA prediction",plotTitle="wOBA vs. kNN prediction")
print(p3)

p <- basic_scatterplot(data=subset(batting.dt,AB>=AB_cutoff),x.col="multinom_wOBA",y.col="wOBA")
print(p)
# ggplotly(p)

p <- basic_scatterplot(data=subset(batting.dt,AB>=AB_cutoff),x.col="lda_wOBA",y.col="wOBA")
print(p)


# next three plots are the same as previous three, but colored by SB/AB
# shows that the first RF model and kNN model underestimate wOBA for faster players
p <- color_scatterplot(data=subset(batting.dt,AB>=AB_cutoff),x.col="rf_wOBA",y.col="wOBA",color.col="SBperAB")
print(p)
# regress wOBA on rf_wOBA and SBperAB, then plot wOBA vs. fitted values to account for this
mod <- lm(wOBA ~ rf_wOBA + SBperAB, data=subset(batting.dt,AB>=AB_cutoff))
df <- data.table(x=mod$fitted.values,y=subset(batting.dt,AB>=AB_cutoff)$wOBA,
                 color=subset(batting.dt,AB>=AB_cutoff)$SBperAB,
                 playerID=subset(batting.dt,AB>=AB_cutoff)$playerID,
                 yearID=subset(batting.dt,AB>=AB_cutoff)$yearID)
p <- color_scatterplot(data=df,x.col="x",y.col="y",color.col="color",xlab="rf_wOBA",ylab="wOBA",
                       colorBarTitle="SBperAB",plotTitle="wOBA vs. rf_wOBA")
print(p)

p <- color_scatterplot(data=subset(batting.dt,AB>=AB_cutoff),x.col="knn_wOBA",y.col="wOBA",color.col="SBperAB")
print(p)

p <- color_scatterplot(data=subset(batting.dt,AB>=AB_cutoff),x.col="lm_wOBA",y.col="wOBA",color.col="SBperAB")
print(p)

p <- color_scatterplot(data=subset(batting.dt,AB>=AB_cutoff),x.col="multinom_wOBA",y.col="wOBA",color.col="SBperAB")
print(p)

p <- color_scatterplot(data=subset(batting.dt,AB>=AB_cutoff),x.col="lda_wOBA",y.col="wOBA",color.col="SBperAB")
print(p)

# plot this year's wOBA vs. last year's wOBA (some correlation but not a lot)
p <- basic_scatterplot(data=plot_subset,x.col="wOBA.prev",y.col="wOBA")
print(p)

# plot current year's wOBA vs. predictions from last year's data
p <- basic_scatterplot(data=plot_subset,x.col="lm_wOBA.prev",y.col="wOBA")
print(p)

p <- basic_scatterplot(data=plot_subset,x.col="rf_wOBA.prev",y.col="wOBA")
print(p)

p <- basic_scatterplot(data=plot_subset,x.col="knn_wOBA.prev",y.col="wOBA")
print(p)

p <- basic_scatterplot(data=plot_subset,x.col="multinom_wOBA.prev",y.col="wOBA")
print(p)

# home runs
p <- basic_scatterplot(data=plot_subset,x.col="HR.prev",y.col="HR")
print(p)

p <- basic_scatterplot(data=plot_subset,x.col="rf_home_run.prev",y.col="HR")
print(p)

p <- basic_scatterplot(data=plot_subset,x.col="knn_home_run.prev",y.col="HR")
print(p)


# for Saberseminar abstract submission ------------------------------------

p1 <- basic_scatterplot(data=plot_subset,x.col="wOBA.prev",y.col="wOBA",
                        xlab="2015 wOBA",ylab="2016 wOBA",plotTitle="2016 wOBA vs. 2015 wOBA")

mod <- lm(wOBA ~ rf_wOBA.prev + SBperAB.prev, data=plot_subset)
df <- data.table(x=mod$fitted.values,y=plot_subset$wOBA,
                 color=plot_subset$SBperAB.prev,
                 playerID=plot_subset$playerID,
                 yearID=plot_subset$yearID)
p2 <- basic_scatterplot(data=df,x.col="x",y.col="y",xlab="2015 random forest wOBA",ylab="2016 wOBA",
                       plotTitle="2016 wOBA vs. 2015 random forest prediction")

library(gridExtra)
grid.arrange(p1,p2,ncol=2)




# future ideas ------------------------------------------------------------
# - more models
# - add wind speed and direction from retrosheet data (it's in event files, maybe in game files)
# - player age/experience
# - use different measures of player value
#   - SLG% would be doable for each batted ball (predict total bases from exit velo, etc.)
# - compare to Marcel projection system
#   - this page describes the methodology: https://www.beyondtheboxscore.com/2016/2/22/11079186/projections-marcel-pecota-zips-steamer-explained-guide-math-is-fun 
# - predict more than wOBA
#   - with class probabilities, can predict number of 1Bs, 2Bs, 3Bs, and HRs
# - use speed score instead of SB/AB
#   - available on FanGraphs: https://www.fangraphs.com/library/offense/spd/ 
# - how to account for the fact that HRs seem more likely to happen in 2017?
# - neural network
#   - exit velocity, launch angle, spray angle
#   - L/R
#   - home team
#   - speed score
#   - game year and month
# - consdier using mixed effects model with some additional variables as random effects
#   - pitcher, opposing team, temperature, inning, etc.
#   - good example of a baseball article using mixed effect models:
#     https://www.baseballprospectus.com/news/article/25514/moving-beyond-wowy-a-mixed-approach-to-measuring-catcher-framing/
# - any way to get confidence intervals for correlation coefficients?
  




# to do -------------------------------------------------------------------
# - create function that adds class probabilities to batted data frame
# - calculate or download Marcel projections
# - download speed scores and incorporate into models
# - look up Bill James aging curve





