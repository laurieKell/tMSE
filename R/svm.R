# library(e1071)
# library(rpart)
# data(Glass, package="mlbench")
# ## split data into a train and test set
# index<- 1:nrow(Glass)
# testindex <- sample(index, trunc(length(index)/3))
# testset<- Glass[testindex,]
# trainset <- Glass[-testindex,]
# 
# svm.model <- svm(Type ~ ., data = trainset, cost = 100, gamma = 1)
# svm.pred <- predict(svm.model, testset[,-10])
# 
# rpart.model <- rpart(Type ~ ., data = trainset)
# rpart.pred <- predict(rpart.model, testset[,-10], type = "class")
#  library(e1071)
# library(rpart)
# data(Ozone, package="mlbench")
# ## split data into a train and test set
# index<- 1:nrow(Ozone)
# testindex <- sample(index, trunc(length(index)/3))
# testset<- na.omit(Ozone[testindex,-3])
# trainset <- na.omit(Ozone[-testindex,-3])
# 
# svm.model <- svm(V4 ~ ., data = trainset, cost = 1000, gamma = 0.0001)
# svm.pred <- predict(svm.model, testset[,-3])
# crossprod(svm.pred - testset[,3]) / length(testindex)
# 
# ## rpart
# rpart.model <- rpart(V4 ~ ., data = trainset)
# rpart.pred <- predict(rpart.model, testset[,-3])
# crossprod(rpart.pred - testset[,3]) / length(testindex)