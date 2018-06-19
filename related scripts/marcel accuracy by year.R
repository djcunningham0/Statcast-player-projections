library(ModelMetrics)

cor.vals <- c()
mae.vals <- c()
rmse.vals <- c()
years <- 1958:2017
for (year in years) {
  print(year)
  eval_df <- get_marcel_eval_df(year, AB_cutoff=100)
  cor.vals <- c(cor.vals, cor(eval_df$wOBA, eval_df$xwOBA))
  mae.vals <- c(mae.vals, mae(eval_df$wOBA, eval_df$xwOBA))
  rmse.vals <- c(rmse.vals, rmse(eval_df$wOBA, eval_df$xwOBA))
}

par(mfrow=c(2,2))
plot(years, cor.vals, type='l')
plot(years, mae.vals, type='l')
plot(years, rmse.vals, type='l')
par(mfrow=c(1,1))
