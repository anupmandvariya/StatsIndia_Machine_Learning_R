### Data Preperation
str(iris)
set.seed(1234) 
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]

######################################
### Decision Tree - Classification ###
######################################
library(party)
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data=trainData)

# check the prediction
table(predict(iris_ctree), trainData$Species)
print(iris_ctree)
plot(iris_ctree)
plot(iris_ctree, type="simple")

# predict on test data
testPred <- predict(iris_ctree, newdata = testData)
table(testPred, testData$Species)


######################################
### Random Forest - Classification ###
######################################

ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]

library(randomForest)
rf <- randomForest(Species ~ ., data=trainData, ntree=100, proximity=TRUE)
table(predict(rf), trainData$Species)
print(rf)
attributes(rf)
plot(rf)

### Variable importance plot
importance(rf)
varImpPlot(rf)

### Predict on Test Data
irisPred <- predict(rf, newdata=testData)
table(irisPred, testData$Species)
plot(margin(rf, testData$Species))

