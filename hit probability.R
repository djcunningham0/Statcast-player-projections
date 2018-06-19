# idea for this plot is from
#   - blog: https://baseballwithr.wordpress.com/2017/11/20/generating-hit-probabilities-from-statcast-data/
#   - github: https://gist.github.com/bayesball/7cbd17b6a1a61e9cc96d453182022080

library(ggplot2)
library(data.table)

df <- expand.grid(launch_angle = seq(-20, 50, length=50),
                  launch_speed = seq(40, 120, length=50),
                  spray_angle = seq(-45,45,length=10))

# preds.rf <- predict(rf, df, type="prob")
# probs.rf <- 1-preds.rf[,"out"]

preds.knn <- predict(knnmod, df, type="prob")
probs.knn <- 1-preds.knn[,"out"]

# df$hit.rf <- probs.rf
df$hit.knn <- probs.knn

df <- data.table(df)
df <- df[,list(#rf=mean(hit.rf), 
               knn=mean(hit.knn)), by=list(launch_angle,launch_speed)]


p <- (ggplot(df, aes(x=launch_angle, y=launch_speed, z=knn))
      + stat_contour(geom="polygon", breaks=seq(0, 1, length.out = 40), aes(fill=..level..)) 
      + scale_fill_gradientn(colours = heat.colors(10),
                             guide=guide_colorbar(title="")
                             ,limits=c(0,1)) 
      + geom_vline(xintercept = 0, color="black") 
      + xlim(-25, 50) + ylim(40, 120) 
      + labs(title="Contour Plot of Hit Probability", x="launch angle (degrees)", y="exit velocity (mph)")
      + theme(plot.title = element_text(hjust = 0.5, size = 28),
              text=element_text(size=20)))
print(p)

set.seed(123)
batted.sub <- batted[sample(nrow(batted), 20000),]
batted.sub <- subset(batted.sub, (hc_y<=hc_x+75 & hc_y<=325-hc_x))
batted.sub$hit <- ifelse(batted.sub$class=="out","out","hit")
p1 <- (ggplot(batted.sub, aes(x=hc_x, y=-hc_y, color=hit))
       + geom_point(alpha=.25, size=2)
       + scale_color_manual(values=c(hcl(195, c=100, l=65),
                                     hcl(15, c=100, l=65)))
       + labs(title="Results of Subset of Batted Balls")
       + guides(color=guide_legend(override.aes=list(alpha=1)))
       + theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank(),
               plot.title = element_text(hjust = 0.5, size = 28),
               text=element_text(size=20),
               legend.title=element_blank(),
               legend.key.size = unit(10,'mm')))
print(p1)


p2 <- (ggplot(batted.sub, aes(x=hc_x, y=-hc_y, color=class))
       + geom_point(alpha=.25, size=2)
       # + scale_color_manual(values=c(hcl(195, c=100, l=65),
       #                               hcl(15, c=100, l=65)))
       + labs(title="Results of Subset of Batted Balls")
       + guides(color=guide_legend(override.aes=list(alpha=1)))
       + theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank(),
               plot.title = element_text(hjust = 0.5, size = 28),
               text=element_text(size=20),
               legend.title=element_blank(),
               legend.key.size = unit(10,'mm')))
print(p2)


