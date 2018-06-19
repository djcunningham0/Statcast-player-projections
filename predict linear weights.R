# functions ---------------------------------------------------------------

source("./define_functions.R")


# prepare data frames -----------------------------------------------------
print("Preparing data frames...")
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

print("Fitting multinomial...")
library(nnet)
mod.multinom <- multinom(class ~ launch_speed + launch_angle + spray_angle, data=batted)
probs.multinom <- predict(mod.multinom,newdata=batted,type="prob")
batted <- add_preds_from_probs("multinom", batted, probs.multinom, lw)

# this made almost no difference
# mod.multinom.shift <- multinom(class ~ launch_speed + launch_angle + spray_angle + shift, data=batted)
# probs <- predict(mod.multinom.shift,newdata=batted,type="prob")
# batted <- add_preds_from_probs("multinom.shift", batted, probs, lw)

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

# library(MASS)
# mod.lda <- lda(class ~ launch_speed + launch_angle + spray_angle, data=batted)
# probs.lda <- predict(mod.lda,newdata=batted)$posterior
# batted <- add_preds_from_probs("lda", batted, probs.lda, lw)


# fit random forest models ------------------------------------------------

# might want to experiment with nodesize > 1 to make fitting faster

print("Fitting random forest...")
### Need to fit new models with all of the data ###
library(randomForest)

load("./rf.RData")
load("./rf.speed.RData")
load("./rf.shift.RData")
set.seed(1)
which.train <- sample(1:dim(batted)[1],1e5)
train <- batted[which.train,]
# rf <- randomForest(class ~ launch_speed + launch_angle + spray_angle, data=train)
# rf.speed <- randomForest(class ~ launch_speed + launch_angle + spray_angle + Spd, data=train)
# rf.shift <- randomForest(class ~ launch_speed + launch_angle + spray_angle + Spd + shift, data=train)

probs.rf <- predict(rf, newdata=batted, type="prob")
batted <- add_preds_from_probs("rf", batted, probs.rf, lw)

probs.rf.speed <- predict(rf.speed, newdata=batted, type="prob")
batted <- add_preds_from_probs("rf.speed", batted, probs.rf.speed, lw)

probs.rf.shift <- predict(rf.shift, newdata=batted, type="prob")
batted <- add_preds_from_probs("rf.shift", batted, probs.rf.shift, lw)

probs.rf.shift.2 <- predict(rf.shift.2, newdata=batted, type="prob")
batted <- add_preds_from_probs("rf.shift.2", batted, probs.rf.shift.2, lw)


# fit kNN model -----------------------------------------------------------

print("Fitting kNN...")
library(caret)

# tune k with 'knn cross validation.R'
# k=50 works well

# currently fitting model on half the data
# could fit on all data, but would need to re-tune k and it won't make much difference
# knnmod <- fit_knn_model(batted, k=50, trainSize=0.5, seed=1)
load("./knnmod.RData")
probs.knn <- predict(knnmod,newdata=batted,type="prob")
probs.knn <- as.matrix(probs.knn)  # it was returning a list, which caused problems with the next function
batted <- add_preds_from_probs("knn", batted, probs.knn, lw)


# fit other models --------------------------------------------------------

# Ideas:
# use methods in caret package: http://topepo.github.io/caret/train-models-by-tag.html#Tree_Based_Model 
# see this website for info and examples for a lot of these: https://www.analyticsvidhya.com/blog/2017/09/common-machine-learning-algorithms/
# - Naive Bayes
# - SVM?
# - XGBoost (boosted trees. use method="xgbTree" or "xgbLinear")
# - somehow account for the fact that the RF model underestimates wOBA for faster players
# - add age into the model, or adjust for future year's prediction
# - mixed effect vesion of multinomial model


print("Done fitting models.")

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

batting_lagged <- lag_yearly_data(batting.dt)
sub.lag <- subset(batting_lagged,AB>=AB_cutoff & AB.prev>=AB_cutoff)


# get Marcel projections --------------------------------------------------

steamer.2017 <- read.csv("../projections/Steamer projections 2017.csv")
steamer.2017 <- add_bbref_and_lahman_ids(steamer.2017)

eval.df.2017 <- get_marcel_eval_df(2017, lw_years=2015:2017, pred_df=batting.dt, AB_cutoff=AB_cutoff)
eval.df.2017 <- add_steamer_to_eval_df(eval.df.2017, steamer.2017)

marcel_eval_plot(eval.df.2017, model_desc="Marcel")
marcel_eval_plot(eval.df.2017, model_prefix="rf", model_desc="RF (w/o Spd)")
marcel_eval_plot(eval.df.2017, model_prefix="rf.speed", model_desc="RF (speed)")
marcel_eval_plot(eval.df.2017, model_prefix="rf.shift", model_desc="RF (shift)")
marcel_eval_plot(eval.df.2017, model_prefix="rf.shift.2", model_desc="RF (shift 2)")
marcel_eval_plot(eval.df.2017, model_prefix="knn", model_desc="kNN")
marcel_eval_plot(eval.df.2017, model_prefix="multinom", model_desc="multinom")
marcel_eval_plot(eval.df.2017, model_prefix="steamer", model_desc="Steamer")

eval.df.2016 <- get_marcel_eval_df(2016, lw_years=2015:2017, pred_df=batting.dt, AB_cutoff=AB_cutoff)
marcel_eval_plot(eval.df.2016, model_desc="Marcel")
marcel_eval_plot(eval.df.2016, model_prefix="rf.speed", model_desc="RF")
marcel_eval_plot(eval.df.2016, model_prefix="rf.shift", model_desc="RF (shift)")
marcel_eval_plot(eval.df.2016, model_prefix="rf", model_desc="RF (w/o speed)")
marcel_eval_plot(eval.df.2016, model_prefix="knn", model_desc="kNN")

# match the analysis here:
#   https://web.archive.org/web/20080111231423/http://www.baseballprospectus.com/unfiltered/?p=564
#   (Nate Silver got 0.591 correlation with OPS for Marcel projections)
# (Not sure why the correlation is so much higher than for 2017. Is Marcel becoming less reliable
#  in the new hitting environment? Are other projection systems also becoming less reliable? Was 2016
#  just a particularly difficult year?)
eval.df.2007 <- get_marcel_eval_df(2007, AB_cutoff=100)
marcel_eval_plot(eval.df.2007, model_desc="Marcel")



# evaluate Marcel projections ---------------------------------------------

summary.2017.wOBA <- create_eval_summary(eval.df.2017)
summary.2017.OPS <- create_eval_summary(eval.df.2017, stat="OPS")

# library(knitr)
# kable(summary.2017.wOBA, digits=3)
# kable(summary.2017.wOBA, digits=3, format="latex")

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
# - should I account for shifts? Statcast data has if_fielding_alignment
  




# to do -------------------------------------------------------------------






