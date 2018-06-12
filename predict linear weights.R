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
# load("./rf_models_032518.RData")
load("./rf_models_060318.RData")

# set.seed(1)
# which.train <- sample(1:dim(batted)[1],1e5)
# train <- batted[which.train,]
# rf <- randomForest(class ~ launch_speed + launch_angle + spray_angle, data=train)
# rf.speed <- randomForest(class ~ launch_speed + launch_angle + spray_angle + Spd, data=train)

probs.rf <- predict(rf, newdata=batted, type="prob")
batted <- add_preds_from_probs("rf", batted, probs.rf, lw)

probs.rf.speed <- predict(rf.speed, newdata=batted, type="prob")
batted <- add_preds_from_probs("rf.speed", batted, probs.rf.speed, lw)

# rf2017.class <- randomForest(class ~ launch_speed + launch_angle + spray_angle, data=batted2017)
# rf2017.class.stand <- randomForest(class ~ launch_speed + launch_angle + spray_angle + stand, data=batted2017)
# rf2017.class.home <- randomForest(class ~ launch_speed + launch_angle + spray_angle + home_team, data=batted2017)

# predict linear weight from classification model by taking dot prodict with linear weights vector
# probs.rf <- predict(rf2017.class,newdata=batted,type="prob")
# batted <- add_preds_from_probs("rf", batted, probs.rf, lw)

# predict linear weight from regression model
# batted$rf2_linear_weight <- predict(rf2017,newdata=batted)




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

lw.prefixes <- get_prefixes(batted, type="lw")
full.prefixes <- get_prefixes(batted, type="full")

library(data.table)
weights.dt <- group_weights_by_year(batted)
weights_by_month.dt <- group_weights_by_year(batted, by_month=TRUE)


# add wOBA to Lahman database ---------------------------------------------

batting.dt <- add_preds_to_yearly_data(weights.dt)#, lw.prefixes, full.prefixes)
# need to do the same for monthly data if I'm going to do month-to-month correlations

AB_cutoff <- 150
sub.oneyear <- subset(batting.dt,AB>=AB_cutoff)

lmod.rf <- lm(wOBA ~ rf_wOBA + Spd, data=sub.oneyear)
lmod.knn <- lm(wOBA ~ knn_wOBA + Spd, data=sub.oneyear)

batting.dt$rf.lmod_wOBA <- predict(lmod.rf, newdata=batting.dt)
batting.dt$knn.lmod_wOBA <- predict(lmod.knn, newdata=batting.dt)

batting_lagged <- lag_yearly_data(batting.dt)
sub.lag <- subset(batting_lagged,AB>=AB_cutoff & AB.prev>=AB_cutoff)


# get Marcel projections --------------------------------------------------

marcel.2017 <- marcel_projections(2017, lw_years=2015:2017)
marcel.2017.rf <- marcel_projections(2017, pred_df=batting.dt, model_prefix="rf", lw_years=2015:2017)
marcel.2017.rf.speed <- marcel_projections(2017, pred_df=batting.dt, model_prefix="rf.speed", lw_years=2015:2017)
marcel.2017.knn <- marcel_projections(2017, pred_df=batting.dt, model_prefix="knn", lw_years=2015:2017)
marcel.2017.multinom <- marcel_projections(2017, pred_df=batting.dt, model_prefix="multinom", lw_years=2015:2017)
marcel.2017.lda <- marcel_projections(2017, pred_df=batting.dt, model_prefix="lda", lw_years=2015:2017)




# future ideas ------------------------------------------------------------
# - more models
# - add wind speed and direction from retrosheet data (it's in event files, maybe in game files)
# - player age/experience
# - compare to Marcel projection system
#   - this page describes the methodology: https://www.beyondtheboxscore.com/2016/2/22/11079186/projections-marcel-pecota-zips-steamer-explained-guide-math-is-fun 
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
# - how to evaluate full projections?
#   - maybe some ideas from here:
#     https://www.beyondtheboxscore.com/2017/1/8/14189138/pecota-zips-steamer-marcel-projection-systems-graded
#   - or here:
#     https://web.archive.org/web/20080111231423/http://www.baseballprospectus.com/unfiltered/?p=564
  




# to do -------------------------------------------------------------------
# - calculate or download Marcel projections
# - look up Bill James aging curve





