library(ggplot2)
library(plotly)


# check wOBA correlation within same year --------------------------------------

p.lm <- basic_scatterplot(data=sub.oneyear,x.col="lm_wOBA",y.col="wOBA",
                          xlab="linear model wOBA prediction",plotTitle="wOBA vs. linear model prediction")
print(p.lm)
# ggplotly(p.lm)

# first few plots are just a gut check
# (make sure the models are correlated with wOBA)
p <- basic_scatterplot(data=sub.oneyear,x.col="rf_wOBA",y.col="wOBA",
                       xlab="random forest wOBA prediction",plotTitle="wOBA vs. RF prediction")
print(p)
# ggplotly(p1)

p <- basic_scatterplot(data=sub.oneyear,x.col="rf.speed_wOBA",y.col="wOBA",
                       xlab="random forest wOBA prediction (speed)",plotTitle="wOBA vs. RF prediction (speed)")
print(p)

p3 <- basic_scatterplot(data=sub.oneyear,x.col="knn_wOBA",y.col="wOBA",
                        xlab="kNN wOBA prediction",plotTitle="wOBA vs. kNN prediction")
print(p3)

p <- basic_scatterplot(data=sub.oneyear,x.col="multinom_wOBA",y.col="wOBA")
print(p)
# ggplotly(p)

p <- basic_scatterplot(data=sub.oneyear,x.col="lda_wOBA",y.col="wOBA")
print(p)


# next three plots are the same as previous three, but colored by speed score
# shows that the first RF model and kNN model underestimate wOBA for faster players
p <- color_scatterplot(data=sub.oneyear,x.col="rf_wOBA",y.col="wOBA",color.col="Spd")
print(p)

# regress wOBA on rf_wOBA and SBperAB, then plot wOBA vs. fitted values to account for this
p <- color_scatterplot(data=sub.oneyear,x.col="rf.lmod_wOBA",y.col="wOBA",color.col="Spd")
print(p)

p <- color_scatterplot(data=sub.oneyear,x.col="rf.speed_wOBA",y.col="wOBA",color.col="Spd")
print(p)

p <- color_scatterplot(data=sub.oneyear,x.col="knn_wOBA",y.col="wOBA",color.col="Spd")
print(p)

# regress wOBA on rf_wOBA and SBperAB, then plot wOBA vs. fitted values to account for this
p <- color_scatterplot(data=sub.oneyear,x.col="knn.lmod_wOBA",y.col="wOBA",color.col="Spd")
print(p)


# check wOBA correlation between years -----------------------------------------

# plot this year's wOBA vs. last year's wOBA (some correlation but not a lot)
p <- basic_scatterplot(data=sub.lag,x.col="wOBA.prev",y.col="wOBA")
print(p)

# plot current year's wOBA vs. predictions from last year's data
p <- basic_scatterplot(data=sub.lag,x.col="lm_wOBA.prev",y.col="wOBA")
print(p)

p <- basic_scatterplot(data=sub.lag,x.col="rf_wOBA.prev",y.col="wOBA")
print(p)

p <- basic_scatterplot(data=sub.lag,x.col="rf.speed_wOBA.prev",y.col="wOBA")
print(p)

p <- basic_scatterplot(data=sub.lag,x.col="rf.lmod_wOBA.prev",y.col="wOBA")
print(p)

p <- basic_scatterplot(data=sub.lag,x.col="knn_wOBA.prev",y.col="wOBA")
print(p)

p <- basic_scatterplot(data=sub.lag,x.col="knn.lmod_wOBA.prev",y.col="wOBA")
print(p)

p <- basic_scatterplot(data=sub.lag,x.col="multinom_wOBA.prev",y.col="wOBA")
print(p)

p <- basic_scatterplot(data=sub.lag,x.col="lda_wOBA.prev",y.col="wOBA")
print(p)


# check other stats -------------------------------------------------------

# home runs
p <- basic_scatterplot(data=sub.lag,x.col="HR.prev",y.col="HR")
print(p)

p <- basic_scatterplot(data=sub.lag,x.col="rf_home_run.prev",y.col="HR")
print(p)

p <- basic_scatterplot(data=sub.lag,x.col="knn_home_run.prev",y.col="HR")
print(p)

p <- basic_scatterplot(data=sub.lag,x.col="multinom_home_run.prev",y.col="HR")
print(p)

# triples (should be affected more by speed)
p <- basic_scatterplot(data=sub.oneyear,x.col="X3B",y.col="rf_triple")
print(p)

p <- basic_scatterplot(data=sub.oneyear,x.col="X3B",y.col="rf.speed_triple")
print(p)

p <- basic_scatterplot(data=sub.oneyear,x.col="X3B",y.col="knn_triple")
print(p)

p <- basic_scatterplot(data=sub.lag,x.col="X3B.prev",y.col="X3B")
print(p)

p <- basic_scatterplot(data=sub.lag,x.col="rf_triple.prev",y.col="X3B")
print(p)

p <- basic_scatterplot(data=sub.lag,x.col="rf.speed_triple.prev",y.col="X3B")
print(p)



# single projection system plots ------------------------------------------

steamer.2017 <- read.csv("./projections/Steamer projections 2017.csv")
steamer.2017 <- add_bbref_and_lahman_ids(steamer.2017)

eval.df.2017 <- get_marcel_eval_df(2017, lw_years=2015:2017, pred_df=batting.dt, AB_cutoff=AB_cutoff)
eval.df.2017 <- add_steamer_to_eval_df(eval.df.2017, steamer.2017)

marcel_eval_plot(eval.df.2017, model_desc="Marcel")
marcel_eval_plot(eval.df.2017, model_prefix="rf", model_desc="RF (w/o Spd)")
marcel_eval_plot(eval.df.2017, model_prefix="rf.speed", model_desc="RF (speed)")
marcel_eval_plot(eval.df.2017, model_prefix="rf.shift", model_desc="RF (shift)")
marcel_eval_plot(eval.df.2017, model_prefix="knn", model_desc="kNN")
marcel_eval_plot(eval.df.2017, model_prefix="multinom", model_desc="multinom")
marcel_eval_plot(eval.df.2017, model_prefix="multinom", model_desc="multinom.shift")
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


# projection system comparison --------------------------------------------

# visualize correlation, MAE, and RMSE in a plot
scaled.2017.wOBA <- scale_eval_summary(summary.2017.wOBA)
scaled.2017.OPS <- scale_eval_summary(summary.2017.OPS)

reshaped.2017.wOBA <- reshape_eval_summary(scaled.2017.wOBA)
reshaped.2017.OPS <- reshape_eval_summary(scaled.2017.OPS)

plot.sub.wOBA <- subset(reshaped.2017.wOBA, method %in% c("marcel","steamer","multinom","rf.speed"))
plot.sub.wOBA$method <- factor(plot.sub.wOBA$method)
levels(plot.sub.wOBA$method) <- c("Marcel","MLR Marcel","RF Marcel","Steamer")
levels(plot.sub.wOBA$variable) <- c("Correlation","MAE","RMSE")

p <- (ggplot(data=plot.sub.wOBA, aes(x=variable, y=value, color=method))
      + geom_point(size=5)
      + labs(x="", y="scaled value", title="Relative Accuracy of \nProjections")
      + theme(legend.title=element_blank())
      + theme(legend.key.size=unit(12,'mm'))
      + theme(text=element_text(size=24))
      + theme(plot.title=element_text(hjust=0.5, size=36))
      + ylim(-1,1)
); print(p)


# for Saberseminar abstract submission ------------------------------------

p1 <- basic_scatterplot(data=sub.lag,x.col="wOBA.prev",y.col="wOBA",
                        xlab="2015 wOBA",ylab="2016 wOBA",plotTitle="2016 wOBA vs. 2015 wOBA")

mod <- lm(wOBA ~ rf_wOBA.prev + SBperAB.prev, data=sub.lag)
df <- data.table(x=mod$fitted.values,y=sub.lag$wOBA,
                 color=sub.lag$SBperAB.prev,
                 playerID=sub.lag$playerID,
                 yearID=sub.lag$yearID)
p2 <- basic_scatterplot(data=df,x.col="x",y.col="y",xlab="2015 random forest wOBA",ylab="2016 wOBA",
                        plotTitle="2016 wOBA vs. 2015 random forest prediction")

library(gridExtra)
grid.arrange(p1,p2,ncol=2)



