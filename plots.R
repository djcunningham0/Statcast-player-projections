library(ggplot2)
library(plotly)


# check wOBA correlation within same year --------------------------------------

p.lm <- create_scatterplot(data=sub.oneyear,x.col="lm_wOBA",y.col="wOBA",
                          xlab="linear model wOBA prediction",plotTitle="wOBA vs. linear model prediction")
print(p.lm)
# ggplotly(p.lm)

# first few plots are just a gut check
# (make sure the models are correlated with wOBA)
p <- create_scatterplot(data=sub.oneyear, x.col="rf_wOBA", y.col="wOBA",
                        xlab="RF wOBA prediction",plotTitle="wOBA vs. RF prediction",
                        center_title=TRUE, point_alpha=0.7); print(p)
# ggplotly(p)

p3 <- create_scatterplot(data=sub.oneyear,x.col="knn_wOBA",y.col="wOBA",
                        xlab="kNN wOBA prediction",plotTitle="wOBA vs. kNN prediction")
print(p3)

p <- create_scatterplot(data=sub.oneyear, x.col="multinom_wOBA", y.col="wOBA", 
                        xlab="MLR wOBA prediction", plotTitle="wOBA vs. MLR prediction",
                        center_title=TRUE, point_alpha=0.7)
print(p)
# ggplotly(p)

p <- create_scatterplot(data=sub.oneyear, x.col="mlbx_wOBA", y.col="wOBA",
                        xlab="MLB xwOBA", plotTitle="wOBA vs. xwOBA",
                        center_title=TRUE, point_alpha=0.7)
print(p)

# next three plots are the same as previous three, but colored by speed score
# shows that the first RF model and kNN model underestimate wOBA for faster players
p <- create_scatterplot(data=sub.oneyear,x.col="rf_wOBA",y.col="wOBA",color.col="Spd")
print(p)

# regress wOBA on rf_wOBA and SBperAB, then plot wOBA vs. fitted values to account for this
p <- create_scatterplot(data=sub.oneyear,x.col="rf_wOBA",y.col="wOBA",color.col="Spd")
print(p)

p <- create_scatterplot(data=sub.oneyear,x.col="knn_wOBA",y.col="wOBA",color.col="Spd")
print(p)

p <- create_scatterplot(data=sub.oneyear,x.col="mlbx_wOBA",y.col="wOBA",color.col="Spd")
print(p)


sub.oneyear$rf.simple_resid <- sub.oneyear$rf.simple_wOBA - sub.oneyear$wOBA

# check wOBA correlation between years -----------------------------------------

# plot this year's wOBA vs. last year's wOBA (some correlation but not a lot)
p <- create_scatterplot(data=sub.lag,x.col="wOBA.prev",y.col="wOBA",
                        xlab="previous season wOBA", plotTitle="wOBA vs. Previous wOBA",
                        center_title=TRUE)
print(p)

# plot current year's wOBA vs. predictions from last year's data
p <- create_scatterplot(data=sub.lag,x.col="lm_wOBA.prev",y.col="wOBA")
print(p)

p <- create_scatterplot(data=sub.lag,x.col="rf_wOBA.prev",y.col="wOBA",
                        xlab="previous season RF wOBA", plotTitle="wOBA vs. Previous RF wOBA",
                        center_title=TRUE)
print(p)

p <- create_scatterplot(data=sub.lag,x.col="mlbx_wOBA.prev",y.col="wOBA",
                        xlab="previous season MLB xwOBA", plotTitle="wOBA vs. Previous MLB xwOBA",
                        center_title=TRUE)
print(p)

p <- create_scatterplot(data=sub.lag,x.col="knn_wOBA.prev",y.col="wOBA")
print(p)

p <- create_scatterplot(data=sub.lag,x.col="multinom_wOBA.prev",y.col="wOBA")
print(p)

# how well do the "expected" stats correlate with themselves?
p <- create_scatterplot(data=sub.lag, x.col="rf_wOBA.prev", y.col="rf_wOBA")
print(p)

p <- create_scatterplot(data=sub.lag, x.col="mlbx_wOBA.prev", y.col="mlbx_wOBA")
print(p)

p <- create_scatterplot(data=sub.lag, x.col="multinom_wOBA.prev", y.col="multinom_wOBA")
print(p)



# check other stats -------------------------------------------------------

# home runs
p <- create_scatterplot(data=sub.lag,x.col="HR.prev",y.col="HR")
print(p)

p <- create_scatterplot(data=sub.lag,x.col="rf_home_run.prev",y.col="HR")
print(p)

p <- create_scatterplot(data=sub.lag,x.col="knn_home_run.prev",y.col="HR")
print(p)

p <- create_scatterplot(data=sub.lag,x.col="multinom_home_run.prev",y.col="HR")
print(p)

# triples (should be affected more by speed)
p <- create_scatterplot(data=sub.oneyear,x.col="X3B",y.col="rf_triple")
print(p)

p <- create_scatterplot(data=sub.oneyear,x.col="X3B",y.col="rf.speed_triple")
print(p)

p <- create_scatterplot(data=sub.oneyear,x.col="X3B",y.col="knn_triple")
print(p)

p <- create_scatterplot(data=sub.lag,x.col="X3B.prev",y.col="X3B")
print(p)

p <- create_scatterplot(data=sub.lag,x.col="rf_triple.prev",y.col="X3B")
print(p)

p <- create_scatterplot(data=sub.lag,x.col="rf.speed_triple.prev",y.col="X3B")
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




# for Saberseminar abstract submission ------------------------------------

p1 <- create_scatterplot(data=sub.lag,x.col="wOBA.prev",y.col="wOBA",
                        xlab="2015 wOBA",ylab="2016 wOBA",plotTitle="2016 wOBA vs. 2015 wOBA")

mod <- lm(wOBA ~ rf_wOBA.prev + SBperAB.prev, data=sub.lag)
df <- data.table(x=mod$fitted.values,y=sub.lag$wOBA,
                 color=sub.lag$SBperAB.prev,
                 playerID=sub.lag$playerID,
                 yearID=sub.lag$yearID)
p2 <- create_scatterplot(data=df,x.col="x",y.col="y",xlab="2015 random forest wOBA",ylab="2016 wOBA",
                        plotTitle="2016 wOBA vs. 2015 random forest prediction")

library(gridExtra)
grid.arrange(p1,p2,ncol=2)



# for MS paper ------------------------------------------------------------

# first four plots for correlation with wOBA
p1 <- create_scatterplot(data=sub.oneyear, x.col="multinom_wOBA", y.col="wOBA",
                         xlab="Multinomial wOBA prediction",plotTitle="wOBA vs. multinomial prediction",
                         center_title=TRUE, point_alpha=0.5,
                         title_size=12, text_size=10, point_size=1); print(p1)

p2 <- create_scatterplot(data=sub.oneyear, x.col="knn_wOBA", y.col="wOBA",
                         xlab="kNN wOBA prediction",plotTitle="wOBA vs. kNN prediction",
                         center_title=TRUE, point_alpha=0.5,
                         title_size=12, text_size=10, point_size=1); print(p2)

p3 <- create_scatterplot(data=sub.oneyear, x.col="rf.simple_wOBA", y.col="wOBA",
                         xlab="RF wOBA prediction",plotTitle="wOBA vs. RF prediction",
                         center_title=TRUE, point_alpha=0.5,
                         title_size=12, text_size=10, point_size=1); print(p3)

p4 <- create_scatterplot(data=sub.oneyear, x.col="mlbx_wOBA", y.col="wOBA",
                         xlab="MLB xwOBA prediction",plotTitle="wOBA vs. MLB xwOBA",
                         center_title=TRUE, point_alpha=0.5,
                         title_size=12, text_size=10, point_size=1); print(p4)

ggsave(plot=p1, width=3.75, height=2.5, units="in", 
       filename="~/Dropbox/UChicago thesis/figures/multinom_vs_woba.png")
ggsave(plot=p2, width=3.75, height=2.5, units="in", 
       filename="~/Dropbox/UChicago thesis/figures/knn_vs_woba.png")
ggsave(plot=p3, width=3.75, height=2.5, units="in", 
       filename="~/Dropbox/UChicago thesis/figures/rf_simple_vs_woba.png")
ggsave(plot=p4, width=3.75, height=2.5, units="in", 
       filename="~/Dropbox/UChicago thesis/figures/xwoba_vs_woba.png")

# rf model colored by speed score
p5 <- create_scatterplot(data=sub.oneyear, x.col="rf.simple_wOBA", y.col="wOBA", color.col="Spd",
                         xlab="RF wOBA prediction",plotTitle="wOBA vs. RF prediction",
                         center_title=TRUE, point_alpha=0.5,
                         title_size=12, text_size=10, point_size=1); print(p5)

sub.oneyear <- sub.oneyear %>% 
  mutate(rf.simple_resid = rf.simple_wOBA - wOBA)
p6 <- (create_scatterplot(data=sub.oneyear, x.col="Spd", y.col="rf.simple_resid",
                         xlab="Speed score", ylab="Random forest wOBA residual", 
                         plotTitle="Random forest wOBA residuals vs. batter speed score",
                         center_title=TRUE, point_alpha=0.7,
                         title_size=12, text_size=10, point_size=1,
                         include_y_equal_x=FALSE)
       + geom_hline(yintercept=0, lty=2, col='red')); print(p6)

ggsave(plot=p5, width=3.75, height=2.5, units="in", 
       filename="~/Dropbox/UChicago thesis/figures/rf_speed.png")
ggsave(plot=p6, width=3.75, height=2.5, units="in", 
       filename="~/Dropbox/UChicago thesis/figures/rf_speed_residuals.png")

# correlation plot of the final model
p7 <- create_scatterplot(data=sub.oneyear, x.col="rf_wOBA", y.col="wOBA",
                         xlab="RF wOBA prediction",plotTitle="wOBA vs. RF prediction",
                         center_title=TRUE, point_alpha=0.5,
                         title_size=12, text_size=10, point_size=1); print(p7)

ggsave(plot=p7, width=4.5, height=3, units="in", 
       filename="~/Dropbox/UChicago thesis/figures/rf_final_vs_woba.png")

# projection accuracy
p <- plot_projection_summary(summary.full.wOBA,
                             which=c("marcel", "steamer", "multinom", "rf", "knn"),
                             names=c("Marcel", "Steamer", "MLR Marcel", "RF Marcel", "k-NN"),
                             # which=c("marcel", "steamer", full.prefixes),
                             # plot.title="Relative Accuracy of\nProjections",
                             subtitle=("(2017-18 wOBA)"),
                             point_size=2,
                             text_size=10,
                             title_size=12,
                             marcel_at_zero=TRUE
); print(p)

ggsave(plot=p, width=4.5, height=3, units="in",
       filename="~/Dropbox/UChicago thesis/figures/projection_eval.png")
