OriginalTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
OriginalTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

NewsTrain = read.csv("AddedNewsTrain.csv", stringsAsFactors = FALSE)
NewsTest = read.csv("AddedNewsTest.csv", stringsAsFactors = FALSE)

#in this part, we want to make a better model, instead of one with higher number of variables 

library(caTools)
set.seed(1)
spl = sample.split (NewsTrain$Popular, SplitRatio = 0.7)

trainSet = subset(NewsTrain, spl == TRUE)
testSet = subset(NewsTrain, spl == FALSE)

library(caret)
library(dplyr)

trainSet$Popular[trainSet$Popular == 1] = "Yes"
trainSet$Popular[trainSet$Popular == 0] = "No"

testSet$Popular[testSet$Popular == 1] = "Yes"
testSet$Popular[testSet$Popular == 0] = "No"

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
objControl <- trainControl(method='cv', 
                           summaryFunction = fiveStats, classProbs= TRUE)

modelCasualglm = train(as.factor(Popular) ~. ,data = trainSet,  method = "gbm",
                       trControl = objControl,
                       metric ="ROC",verbose = FALSE)

outcome = "Popular"
modelCasualglm$results
PredTest = predict(modelCasualglm, testSet, type = "prob")

table(PredTest$Yes > 0.5,testSet[,outcome])
#0.9107143





library(randomForest)
modelRF = randomForest(as.factor(Popular)~., data = trainSet)
PredTest = predict(modelRF,testSet)
table(PredTest,testSet[,outcome])
#0.9102041




modelglm = glm (as.factor(Popular)~., data = trainSet,family = binomial)
PredTest = predict(modelglm, testSet)
table(PredTest > 0.5,testSet[,outcome])
#0.9035714




objControl <- trainControl(method='cv', 
                           summaryFunction = fiveStats, classProbs= TRUE)

modelCasualglm = train(as.factor(Popular) ~. ,data = trainSet,  method = "rf",
                       trControl = objControl,
                       metric ="ROC",verbose = FALSE)

objControl <- trainControl(method='cv', 
                           summaryFunction = fiveStats, classProbs= TRUE)



PredTest = predict(modelCasualglm, NewsTest, type = "prob")[,2]
MySubmission = data.frame(UniqueID = OriginalTest$UniqueID, Probability1 = PredTest)
write.csv(MySubmission, "SubmissionHeadlinegbm.csv", row.names=FALSE)
write.csv(MySubmission, "SubmissionHeadlinerftrained.csv", row.names=FALSE)
write.csv(MySubmission, "SubmissionHeadlinetreebag.csv",row.names= FALSE)
write.csv(MySubmission, "SubmissionHeadlineGLMBoost.csv",row.names = FALSE)
#0.8994898