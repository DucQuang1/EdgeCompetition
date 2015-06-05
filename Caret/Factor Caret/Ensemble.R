#Ensembles
library(caTools)
set.seed(14)
spl = sample.split (TrainHeadline$Popular, SplitRatio = 0.7)

trainSet = subset(TrainHeadline, spl == TRUE)
testSet = subset(TrainHeadline, spl == FALSE)

library(dplyr)
testSet = filter (testSet,  (SectionName !="Sports" & SectionName != "Style"))

library(caret)
objControl = trainControl(method = 'cv',number = 10,classProbs= TRUE)

modelCasualglm = train(as.factor(Popular) ~. ,data = trainSet,  method = "gbm",
                       trControl = objControl,
                       metric ="ROC",verbose = FALSE)
Predgbm = as.data.frame(predict(modelCasualglm, newdata=testSet, type = "prob"))


table(Predgbm$Yes > 0.5, testSet$Popular)
#0.9193054

library(randomForest)
HeadlineWordsRF = randomForest(as.factor(Popular) ~ ., data= trainSet)
PredRF = as.data.frame(predict(HeadlineWordsRF, newdata=testSet, type = "prob"))

table(PredRF$Yes >0.5,testSet$Popular)

##0.9147089 

HeadlineWordsLog = glm(as.factor(Popular)~., data = trainSet, family= binomial)
PredLog = as.data.frame(predict(HeadlineWordsLog, newdata=testSet, type = "response"))
table(PredLog > 0.5, testSet$Popular)

#0.9004086

#outcome = "Popular"
#predictor = colnames (trainSet)
#predictor = predictor[predictor != "Popular"]
library(e1071)
HeadlineSvm = svm(as.factor(Popular)~., data = trainSet, probability = TRUE)
PredSvm = predict(HeadlineSvm, newdata=testSet,probability = TRUE)
#head(attr(PredSvm, "probabilities"))
table(attr(PredSvm, "probabilities")[,1] >0.5, testSet$Popular)
#0.8968335


library(nnet)
Headlinennet = nnet(as.factor(Popular)~., data= trainSet, size = 10)
Prednnet = predict (Headlinennet, newdata = testSet, type = "raw")
table(Prednnet > 0.5, testSet$Popular)

PredTest = (5*PredLog+2.5*Predgbm$Yes +2.5*PredRF$Yes )/10
#+2.5*attr(PredSvm,"probabilities")[,1]
table(PredTest > 0.5,testSet$Popular)
#0.9065373 increase!

PredTest = (3*Predgbm$Yes +3*PredRF$Yes +4*attr(PredSvm, "probabilities")[,1] )/10
table(PredTest > 0.5,testSet$Popular)
#0.9080695






##### combiinations 
library(randomForest)
HeadlineWordsRF = randomForest(as.factor(Popular)~., data = TrainHeadline, importance = TRUE)
PredRF = as.data.frame(predict(HeadlineWordsRF, newdata=TestHeadline, type = "prob"))

library(caret)
objControl = trainControl(method = 'cv',number = 7,classProbs= TRUE)

modelCasualglm = train(as.factor(Popular) ~. ,data = TrainHeadline,  method = "gbm",
                       trControl = objControl,
                       metric ="ROC",verbose = FALSE)
Predgbm = as.data.frame(predict(modelCasualglm, newdata=TestHeadline, type = "prob"))


HeadlineWordsLog = glm(as.factor(Popular)~., data = TrainHeadline, family= binomial)
PredLog = as.data.frame(predict(HeadlineWordsLog, newdata=TestHeadline, type = "response"))


library(e1071)
HeadlineSvm = svm(as.factor(Popular)~., data = TrainHeadline, probability = TRUE)
PredSvm = predict(HeadlineSvm, newdata=TestHeadline,probability = TRUE)

library(nnet)
Headlinennet = nnet(as.factor(Popular)~., data= TrainHeadline, size = 9)
Prednnet = predict (Headlinennet, newdata = TestHeadline, type = "raw")

PredTest = (2*PredLog+3*Predgbm$Yes +1.5*PredRF$Yes +1.5*attr(PredSvm, "probabilities")[,1] +2*Prednnet )/10
PredTest = (5*PredRF$Yes + 5*Predgbm$Yes)/10
PredTest = (5*attr(PredSvm, "probabilities")[,1] + 5*PredLog)/10
#PredTest = Predgbm
MySubmissionCombined = data.frame(UniqueID = OriginalTest$UniqueID, Probability1 = PredTest)
colnames(MySubmissionCombined) = c("UniqueID","Probability1")
write.csv(MySubmissionCombined, "SubmissionHeadlinecombined.csv", row.names=FALSE)


MySubmissionOverfit = data.frame(UniqueID = OriginalTest$UniqueID, Probability1 = PredTest)
colnames(MySubmissionOverfit) = c("UniqueID","Probability1")
write.csv(MySubmissionOverfit, "SubmissionHeadlineoverfit.csv", row.names=FALSE)
