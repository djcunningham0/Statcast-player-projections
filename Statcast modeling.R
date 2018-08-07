# functions ---------------------------------------------------------------

source("./define_functions.R")


# prepare data frames -----------------------------------------------------
print("Preparing data frames...")

# .rds file is updated by update_data_files.R
if (!exists("original_batted") || !is.data.frame(get("original_batted"))) {
  tryCatch({original_batted <- readRDS("./data/completed_seasons_statcast_batted_balls.rds")},
           error=function(err) {
             print("Missing Statcast data file.")
           })
}

batted <- format_data_frame(original_batted, lw_year=2015:2017)


# fit OLS model --------------------------------------------------------
# lmod <- lm(linear_weight ~ launch_speed + launch_angle + spray_angle, data=batted)
# batted$lm_linear_weight <- predict(lmod, newdata=batted)
# Note: OLS model only predicts linear weight (no class probabilities) and isn't very good (as expected)


# fit multinomial logistic regression model ----------------------------------------

print("Fitting multinomial...")
library(nnet)
# mod.multinom <- multinom(class ~ launch_speed + launch_angle + spray_angle, data=batted)
# saveRDS(mod.multinom, file="./models/multinom.rds")
mod.multinom <- readRDS("./models/multinom.rds")
probs.multinom <- predict(mod.multinom, newdata=batted, type="prob")
batted <- add_preds_from_probs(batted, "multinom", probs.multinom)

# Note: additional features (e.g., speed scores) did not improve the multinomial model


# fit random forest models ------------------------------------------------

print("Fitting random forest...")
library(randomForest)

# here's how the models were trained:
# set.seed(1)
# which.train <- sample(1:dim(batted)[1], 2e5)
# train <- batted[which.train,]
# rf <- randomForest(class ~ launch_speed + launch_angle + spray_angle + Spd + home_team, data=train)

rf <- readRDS("./models/rf.rds")
probs.rf <- predict(rf, newdata=batted, type="prob")
batted <- add_preds_from_probs(batted, "rf", probs.rf)


# Note: I tried adding defensive shifts, but it didn't make much difference (actually slightly worse)
# rf.shift <- randomForest(class ~ launch_speed + launch_angle + spray_angle + Spd + shift, data=train)


# fit kNN model -----------------------------------------------------------

# print("Fitting kNN...")
# library(caret)

# tune k with 'knn cross validation.R'
# k=50 works well

# here's how the model was trained:
# currently fitting model on half the data
# could fit on all data, but would need to re-tune k and it won't make much difference
# knnmod <- fit_knn_model(batted, k=50, trainSize=0.5, seed=1)

knnmod <- readRDS("./models/knn.rds")
probs.knn <- predict(knnmod,newdata=batted,type="prob")
probs.knn <- as.matrix(probs.knn)  # it was returning a list, which caused problems with the next function
batted <- add_preds_from_probs(batted, "knn", probs.knn)


# fit other models --------------------------------------------------------

# potentially try more models in the future
print("Done fitting models.")


# confusion matrix --------------------------------------------------------

print("Building confusion matrix...")
preds.multinom <- predict(mod.multinom, newdata=batted)
preds.rf <- predict(rf, newdata=batted)

order <- c("out","single","double","triple","home_run")
batted$class <- factor(batted$class, levels=order)
preds.multinom <- factor(preds.multinom, levels=order)
preds.rf <- factor(preds.rf, levels=order)

mx.multinom <- caret::confusionMatrix(preds.multinom, batted$class)
mx.rf <- caret::confusionMatrix(preds.rf, batted$class)


# group linear weights by player ------------------------------------------

# these are the defaulst in the next function (just leaving here for easy access)
lw.prefixes <- get_prefixes(batted, type="lw")
full.prefixes <- get_prefixes(batted, type="full")

weights.df <- group_weights_by_year(batted)
# weights_by_month.df <- group_weights_by_year(batted, by_month=TRUE)


# add wOBA to Lahman database ---------------------------------------------

batting.df <- add_preds_to_yearly_data(weights.df)
# need to do the same for monthly data if I'm going to do month-to-month correlations

AB_cutoff <- 150
sub.oneyear <- batting.df %>% 
  filter(AB >= AB_cutoff)

batting_lagged <- lag_yearly_data(batting.df)
sub.lag <- batting_lagged %>% 
  filter(AB >= AB_cutoff & AB.prev >= AB_cutoff)


# get Marcel projections --------------------------------------------------

print("Creating Marcel projections...")

# 2018 projections
eval.df.2018 <- get_marcel_eval_df(2018, lw_years=2015:2017, pred_df=batting.df, include_true_stats=FALSE)

# 2017 projections (for evaluation)
steamer.2017 <- read_csv("./projections/Steamer projections 2017.csv", col_types=cols()) %>% 
  rename("X1B" = "1B", "X2B" = "2B", "X3B" = "3B",
         "key_mlbam" = "mlbamid")
eval.df.2017 <- get_marcel_eval_df(2017, lw_years=2015:2017, pred_df=batting.df, AB_cutoff=AB_cutoff)
eval.df.2017 <- add_steamer_to_eval_df(eval.df.2017, steamer.2017)

marcel_eval_plot(eval.df.2017, model_desc="Marcel")
marcel_eval_plot(eval.df.2017, model_prefix="rf", model_desc="RF")
marcel_eval_plot(eval.df.2017, model_prefix="knn", model_desc="kNN")
marcel_eval_plot(eval.df.2017, model_prefix="multinom", model_desc="multinom")
marcel_eval_plot(eval.df.2017, model_prefix="steamer", model_desc="Steamer")

# 2016 projections
# eval.df.2016 <- get_marcel_eval_df(2016, lw_years=2015:2017, pred_df=batting.df, AB_cutoff=AB_cutoff)
# marcel_eval_plot(eval.df.2016, model_desc="Marcel")
# marcel_eval_plot(eval.df.2016, model_prefix="rf", model_desc="RF")
# marcel_eval_plot(eval.df.2016, model_prefix="knn", model_desc="kNN")

# 2007 projections
# match the analysis here:
#   https://web.archive.org/web/20080111231423/http://www.baseballprospectus.com/unfiltered/?p=564
#   (Nate Silver got 0.591 correlation with OPS for Marcel projections)
# (Not sure why the correlation is so much higher than for 2017. Is Marcel becoming less reliable
#  in the new hitting environment? Are other projection systems also becoming less reliable? Was 2016
#  just a particularly difficult year?)
# eval.df.2007 <- get_marcel_eval_df(2007, AB_cutoff=100)
# marcel_eval_plot(eval.df.2007, model_desc="Marcel")



# evaluate Marcel projections ---------------------------------------------

summary.2017.wOBA <- create_eval_summary(eval.df.2017)
summary.2017.OPS  <- create_eval_summary(eval.df.2017, stat="OPS")
summary.2017.OBP  <- create_eval_summary(eval.df.2017, stat="OBP")
summary.2017.SLG  <- create_eval_summary(eval.df.2017, stat="SLG")


# projection system comparison --------------------------------------------

# visualize correlation, MAE, and RMSE in a plot
p <- plot_projection_summary(summary.2017.wOBA,
                             which=c("marcel", "steamer", "multinom", "rf"), 
                             names=c("Marcel", "Steamer", "MLR Marcel", "RF Marcel"),
                             # plot.title="Relative Accuracy of\nProjections",
                             # subtitle=NULL
                             ); print(p)

p <- plot_projection_summary(summary.2017.OPS,
                             which=c("marcel", "steamer", "multinom", "rf"), 
                             names=c("Marcel", "Steamer", "MLR Marcel", "RF Marcel")); print(p)

p <- plot_projection_summary(summary.2017.OBP,
                             which=c("marcel", "steamer", "multinom", "rf"), 
                             names=c("Marcel", "Steamer", "MLR Marcel", "RF Marcel")); print(p)

p <- plot_projection_summary(summary.2017.SLG,
                             which=c("marcel", "steamer", "multinom", "rf"), 
                             names=c("Marcel", "Steamer", "MLR Marcel", "RF Marcel")); print(p)

library(knitr)
kable(summary.2017.wOBA, digits=3)
kable(scale_eval_summary(summary.2017.wOBA), digits=3)
# kable(summary.2017.wOBA, digits=3, format="latex")
