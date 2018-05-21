#' kNN cross validation
#' 
#' Chooses optimal k based on cross validation
#' 
#' @param df data frame to use for fitting the model (no labels)
#' @param labels vector of labels for data in df
#' @param trials number of cross-validation trials to perform
#' @param numSubsets number of sets to divide data (one used as test set in each trial)
#' @param kVals values of k to use when fitting kNN models
#' @param seed if set, set the seed to this value
#' @param verbose set to positive value to output progess information
#' 
#' @return the misclassification rate on the test set
#' 
knn_cross_val <- function(df, labels, trials, numSubsets, kVals, seed=NULL, verbose=0, time=0) {
  if (time >= 1) {
    require(tictoc)
    tic("total")
  }
  require(class)
  if (!is.null(seed)) { set.seed(seed) }
  
  # store misclassification rates in data frames
  # rows will be k values, columns will be trials
  kVals <- sort(unique(kVals))
  misclass.train.df <- data.frame(t1=kVals)
  misclass.test.df <- data.frame(t1=kVals)
  rownames(misclass.train.df) <- paste0("k",kVals)
  rownames(misclass.test.df) <- paste0("k",kVals)
  
  n <- dim(df)[1]
  for (i in 1:trials) {
    if (time >= 1) { tic(paste0("trial ",i)) }
    if (verbose>=1) { print(paste0("trial ",i)) }
    which.test <- sample(n, floor(n/numSubsets), replace=FALSE)
    
    # create test and training data sets
    test.df <- df[which.test,]    # test set is random sample of size n/numSubsets
    train.df <- df[-which.test,]  # training set is everything else
    
    # get labels for test and training sets
    test.labels <- labels[which.test]
    train.labels <- labels[-which.test]
    
    # fit models and compute misclassification rates
    misclass.train <- c()
    misclass.test <- c()
    for (j in 1:length(kVals)) {
      if (time >= 2) { tic(paste0("trial ",i,", k=",kVals[j])) }
      k <- kVals[j]
      if (verbose>=2) { print(paste0("k=",k))}
      
      # fit model
      all.preds <- knn(train=train.df,test=df,cl=factor(train.labels),k=k)
      train.preds <- all.preds[-which.test]
      test.preds <- all.preds[which.test]
      
      # compute misclassification rates
      misclass.train[j] <- knn_misclass(train.preds, train.labels)
      misclass.test[j] <- knn_misclass(test.preds, test.labels)
      
      if (time >= 2) { toc() }
    }
    misclass.train.df[,paste0("t",i)] <- misclass.train
    misclass.test.df[,paste0("t",i)] <- misclass.test
    
    if (time >= 1) { toc() }
  }  # finish loop through trials
  
  # compute average train and test misclassification rates
  # (average of rows in misclass data frames)
  avg.train.misclass <- c()
  avg.test.misclass <- c()
  for (row in 1:length(kVals)) {
    avg.train.misclass[row] <- mean(t(misclass.train.df)[,row])  # transpose to avoid errors
    avg.test.misclass[row] <- mean(t(misclass.test.df)[,row])
  }
  
  # plot train and test misclassification rates
  if (time >= 1) { tic("plot") }
  require(ggplot2)
  p <- (ggplot(data.frame(k=kVals,train=avg.train.misclass,test=avg.test.misclass),aes(x=k))
        + geom_point(aes(y=train,color="train")) + geom_point(aes(y=test,color="test"))
        + scale_colour_manual(values=c('train'='red','test'='blue'))
        + scale_x_continuous(breaks=kVals)
        + labs(y="error rate")
  )
  print(p)
  if (time >= 1) { toc() }  # plot
  if (time >= 1) { toc() }  # total
}

#' kNN misclassification rate
#' 
#' Calculates the misclassification rate of a kNN model given the test set.
#' 
#' @param model.preds a kNN model fit with class::knn
#' @param test.labels the labels for the test set used to fit the model
#' 
#' @return the misclassification rate on the test set
#' 
knn_misclass <- function(model.preds,test.labels) {
  return(sum(test.labels != model.preds) / length(test.labels))
}

# print(Sys.time())
set.seed(1)
n <- dim(batted)[1]
which.train <- sample(1:n,floor(n*.5))
training <- batted[which.train,]
training <- training[c("class","launch_speed","launch_angle","spray_angle","linear_weight")]
knn_cross_val(df=training[,c("launch_speed","launch_angle","spray_angle")], labels=training$class,
              trials=10, numSubsets=10, kVals=c(5,7,9), seed=1, verbose=0, time=2)
print(Sys.time())





