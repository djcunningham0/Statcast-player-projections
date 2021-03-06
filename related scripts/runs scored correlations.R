source("./define_functions.R")

library(Lahman)
library(baseballr)

data(Teams)
Teams <- subset(Teams, yearID>1900)

Teams$win_perc <- with(Teams, W/(W+L))
Teams$RperG <- with(Teams,R/G)
Teams$SF[is.na(Teams$SF)] <- 0
Teams$HBP[is.na(Teams$HBP)] <- 0

Teams$PA <- with(Teams, AB+BB+SF+HBP)
Teams$BA  <- with(Teams, H/AB)
Teams$OBP <- with(Teams, (H+BB+HBP)/PA)
Teams$X1B <- with(Teams, H-X2B-X3B-HR)
Teams$SLG <- with(Teams, (X1B + 2*X2B + 3*X3B + 4*HR)/AB)
Teams$OPS <- with(Teams, OBP+SLG)

lw.df <- fg_guts()
n <- dim(Teams)[1]
wOBA.vals <- rep(0,n)
for (i in 1:n) {
  lw <- subset(lw.df, season==Teams[i,"yearID"])
  wOBA.vals[i] <- with(lw, (w1B*Teams[i,"X1B"] + w2B*Teams[i,"X2B"] + w3B*Teams[i,"X3B"]
                            + wHR*Teams[i,"HR"] + wBB*Teams[i,"BB"] + wHBP*Teams[i,"HBP"]) / Teams[i,"PA"])
}
Teams$wOBA <- wOBA.vals
Teams$playerID <- Teams$teamID

alpha <- 0.5
center_title <- TRUE
title_size <- 8
text_size <- 6
point_size <- 0.5
p1 <- create_scatterplot(Teams, x.col="RperG", y.col="BA", xlab="R/G", ylab="BA",
                         plotTitle="Team Batting Average vs. Runs per Game", 
                         text.cols=NULL, point_alpha=alpha, center_title=center_title, 
                         title_size=title_size, text_size=text_size, point_size=point_size)
p2 <- create_scatterplot(Teams, x.col="RperG", y.col="OBP", xlab="R/G", ylab="OBP",
                         plotTitle="Team OBP vs. Runs per Game",
                         text.cols=NULL, point_alpha=alpha, center_title=center_title, 
                         title_size=title_size, text_size=text_size, point_size=point_size)
p3 <- create_scatterplot(Teams, x.col="RperG", y.col="SLG", xlab="R/G", ylab="SLG%",
                         plotTitle="Team SLG% vs. Runs per Game",
                         text.cols=NULL, point_alpha=alpha, center_title=center_title, 
                         title_size=title_size, text_size=text_size, point_size=point_size)
p4 <- create_scatterplot(Teams, x.col="RperG", y.col="OPS", xlab="R/G", ylab="OPS",
                         plotTitle="Team OPS vs. Runs per Game",
                         text.cols=NULL, point_alpha=alpha, center_title=center_title, 
                         title_size=title_size, text_size=text_size, point_size=point_size)
p5 <- create_scatterplot(Teams, x.col="RperG", y.col="wOBA", xlab="R/G", ylab="wOBA",
                         plotTitle="Team wOBA vs. Runs per Game",
                         text.cols=NULL, point_alpha=alpha, center_title=center_title, 
                         title_size=title_size, text_size=text_size, point_size=point_size)

# save as images for paper
dpi <- 500
ggsave(plot=p1, filename="~/Dropbox/UChicago thesis/figures/ba_vs_runs.png", dpi=dpi, height=2, width=3)
ggsave(plot=p2, filename="~/Dropbox/UChicago thesis/figures/obp_vs_runs.png", dpi=dpi, height=2, width=3)
ggsave(plot=p3, filename="~/Dropbox/UChicago thesis/figures/slg_vs_runs.png", dpi=dpi, height=2, width=3)
ggsave(plot=p4, filename="~/Dropbox/UChicago thesis/figures/ops_vs_runs.png", dpi=dpi, height=2, width=3)
ggsave(plot=p5, filename="~/Dropbox/UChicago thesis/figures/woba_vs_runs.png", dpi=dpi, height=2, width=3)

# display this plot
# (exclude SLG% so we get a 2x2 grid)
library(gridExtra)
grid.arrange(p1,p2,p4,p5)
