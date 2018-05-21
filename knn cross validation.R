library(caret)
set.seed(1)
n <- dim(batted)[1]
which.train <- sample(1:n,floor(n*.5))
training <- batted[which.train,]
training <- training[c("class","launch_speed","launch_angle","spray_angle","linear_weight")]

trnCtrl <- trainControl(method="repeatedcv",number=5,repeats=2,verboseIter=TRUE)
kVals <- seq(30,90,10)
knnmod <- train(class ~ ., data=training[c("class","launch_speed","launch_angle","spray_angle")],
                method="knn", trControl=trnCtrl, tuneGrid=expand.grid(k=kVals),
                preProcess=c("scale","center"))

# this works the same as predicting class probabilities then multiplying by linear weights
knnmod.reg <- train(linear_weight ~ ., data=training[c("launch_speed","launch_angle","spray_angle","linear_weight")],
                    method="knn", trControl=trnCtrl, tuneGrid=expand.grid(k=kVals),
                    preProcess=c("scale","center"))

knnmod.reg.full <- train(linear_weight ~ ., data=batted[c("launch_speed","launch_angle","spray_angle","linear_weight")],
                         method="knn", trControl=trnCtrl, tuneGrid=expand.grid(k=c(50,60,70)),
                         preProcess=c("scale","center"))

