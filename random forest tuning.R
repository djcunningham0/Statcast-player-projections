library(randomForest)
library(tictoc)
library(tidyverse)

display_time <- function(elapsed) {
  elapsed <- as.numeric(elapsed)
  if (elapsed < 60) { out <- paste(round(elapsed), "seconds") }
  else if (elapsed < 3600) { out <- paste(floor(elapsed / 60), "minutes,",
                                          round(elapsed %% 60), "seconds")}
  else { out <- paste(floor(elapsed / 3600), "hours",
                      floor((elapsed %% 3600) / 60), "minutes",
                      round((elapsed %% 3600) %% 60), "seconds")}
  return(out)
}

source("./define_functions.R")

# .rds file is updated by update_data_files.R
if (!exists("original_batted") || !is.data.frame(get("original_batted"))) {
  tryCatch({original_batted <- readRDS("./data/completed_seasons_statcast_batted_balls.rds")},
           error=function(err) {
             print("Missing Statcast data file.")
           })
}

batted <- format_data_frame(original_batted, lw_year=2015:2017)

vals <- expand.grid(
  # ntree=seq(100, 800, 100)
  ntree=500  # default
  # ,nodesize=seq(1, 25, 2)
  ,nodesize=1  # default
  ,mtry=c(1, 2, 3, 4, 5)
  # ,mtry=2  # default
  ,seed=c(1,2,3)
)

conf <- list()
f1 <- rep(0, nrow(vals))
all.f1 <- list()
kappa <- rep(0, nrow(vals))
acc <- rep(0, nrow(vals))
time <- rep(0, nrow(vals))

for (i in 1:nrow(vals)) {
  tic()
  
  set.seed(vals[i,"seed"])
  which.train <- sample(1:dim(batted)[1], 2e5)
  train <- batted[which.train,]
  test <- batted[-which.train,]
  
  cat("Starting ", i, ": ", 
      "ntree=", vals[i,"ntree"], ", ",
      "nodesize=", vals[i,"nodesize"], ", ",
      "mtry=", vals[i,"mtry"], ", ",
      "seed=", vals[i,"seed"],
      "... ", sep="")
  
  rf <- randomForest(class ~ launch_speed + launch_angle + spray_angle + Spd + home_team, data=train,
                     ntree=vals[i,"ntree"], nodesize=vals[i,"nodesize"], mtry=vals[i,"mtry"])
  
  # saveRDS(rf, file=paste0("./tmp/rf_ntree=", vals[i,"ntree"], "_nodesize=", vals[i,"nodesize"],
  #                         "_mtry=", vals[i,"mtry"], "_seed=", vals[i,"seed"], ".rds"))

  preds.rf <- predict(rf, newdata=test)  # predict on test set
  order <- c("out","single","double","triple","home_run")
  test$class <- factor(test$class, levels=order)
  preds.rf <- factor(preds.rf, levels=order)
  
  # store confusion matrix for each model
  mx <- caret::confusionMatrix(preds.rf, test$class)
  conf[[i]] <- mx  
  
  # store f1 score for each model (overall and by class)
  out <- f1_score(preds.rf, test$class)
  f1[i] <- out$f1
  all.f1[[i]] <- out$all_f1
  
  # store kappa value and overall accuracy
  kappa[i] <- mx$overall["Kappa"]
  acc[i] <- mx$overall["Accuracy"]
  
  # store the elapsed time for each model (not accurate if the session gets interupted)
  done <- toc(quiet=TRUE)
  elapsed <- as.numeric(done$toc - done$tic)
  time[i] <- as.numeric(elapsed)
  cat(display_time(elapsed), "\n")
}

print("Done.")

# save(vals, f1, kappa, acc, time, all.f1, conf, file="./f1_scores_mtry_10232018.RData")


# ntree
load("./f1_scores_ntree_10222018.RData")

vals$f1 <- f1
vals$kappa <- kappa
vals$acc <- acc
vals$time <- time

summ.ntree <- vals %>%
  group_by(ntree) %>%
  summarize(f1 = mean(f1),
            kappa = mean(kappa),
            acc = mean(acc),
            time = mean(time))

p <- (summ.ntree %>%
        gather(key="metric", value="value", f1, kappa, acc, time) %>%
        filter(!(metric %in% c("time", "f1"))) %>%
        ggplot(aes(x=ntree, y=value, color=metric)) +
        geom_line() + 
        geom_point() + 
        labs(title="Classification accuracy vs. ntree") +
        theme(plot.title = element_text(hjust = 0.5)) + 
        scale_color_discrete(breaks=c("acc", "kappa"),
                             labels=c("Accuracy", "Kappa"))
      ); print(p)

ggsave(plot=p, height=3, width=5, units="in",
       filename="~/Dropbox/UChicago thesis/figures/rf_ntree.png")

# p <- (ggplot(vals, aes(x=ntree, y=acc, color=factor(seed)))
#       + geom_point()); print(p)


# mtry
load("./f1_scores_mtry_10232018.RData")

vals$f1 <- f1
vals$kappa <- kappa
vals$acc <- acc
vals$time <- time

summ.mtry <- vals %>%
  group_by(mtry) %>%
  summarize(f1 = mean(f1),
            kappa = mean(kappa),
            acc = mean(acc),
            time = mean(time))

p <- (summ.mtry %>%
        gather(key="metric", value="value", f1, kappa, acc, time) %>%
        filter(!(metric %in% c("time", "f1"))) %>%
        ggplot(aes(x=mtry, y=value, color=metric)) +
        geom_line() + 
        geom_point() + 
        labs(title="Classification accuracy vs. mtry") +
        theme(plot.title = element_text(hjust = 0.5)) + 
        scale_color_discrete(breaks=c("acc", "kappa"),
                             labels=c("Accuracy", "Kappa"))
      ); print(p)

ggsave(plot=p, height=3, width=5, units="in",
       filename="~/Dropbox/UChicago thesis/figures/rf_mtry.png")

# p <- (ggplot(vals, aes(x=mtry, y=acc, color=factor(seed)))
#       + geom_point()); print(p)

# nodesize
load("./f1_scores_nodesize_10172018.RData")

vals$f1 <- f1
vals$kappa <- kappa
vals$acc <- acc
vals$time <- time
vals$seed <- 1

summ.nodesize <- vals %>%
  group_by(nodesize) %>%
  summarize(f1 = mean(f1),
            kappa = mean(kappa),
            acc = mean(acc),
            time = mean(time))

p <- (summ.nodesize %>%
        gather(key="metric", value="value", f1, kappa, acc, time) %>%
        filter(!(metric %in% c("time", "f1"))) %>%
        ggplot(aes(x=nodesize, y=value, color=metric)) +
        geom_line() + 
        geom_point() + 
        labs(title="Classification accuracy vs. nodesize") +
        theme(plot.title = element_text(hjust = 0.5)) + 
        scale_color_discrete(breaks=c("acc", "kappa"),
                             labels=c("Accuracy", "Kappa"))
      ); print(p)

ggsave(plot=p, height=3, width=5, units="in",
       filename="~/Dropbox/UChicago thesis/figures/rf_nodesize.png")

# p <- (ggplot(vals, aes(x=nodesize, y=acc, color=factor(seed)))
#       + geom_point()); print(p)

