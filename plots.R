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



