#True Ensembles
library(caTools)
set.seed(20)
spl = sample.split (TrainHeadline$Popular, SplitRatio = 0.3)

ensembleData = subset(TrainHeadline, spl == TRUE)
blenderData = subset(TrainHeadline, spl == FALSE)
testingData = TestHeadline


library(randomForest)
HeadlineWordsRF = randomForest(as.factor(Popular)~., data = ensembleData, importance = TRUE)

library(caret)
objControl = trainControl(method = 'cv',number = 10,classProbs= TRUE)

modelCasualglm = train(as.factor(Popular) ~. ,data = ensembleData,  method = "gbm",
                       trControl = objControl,
                       metric ="ROC",verbose = FALSE)


HeadlineWordsLog = glm(as.factor(Popular)~., data = ensembleData, family= binomial)


library(e1071)
HeadlineSvm = svm(as.factor(Popular)~., data = ensembleData, probability = TRUE)

library(nnet)
Headlinennet = nnet(as.factor(Popular)~., data= ensembleData, size = 9)




blenderData$PredRF = predict(HeadlineWordsRF, newdata=blenderData, type = "prob")[,2]
blenderData$Predgbm = predict(modelCasualglm, newdata=blenderData, type = "prob")[,2]
blenderData$PredLog = predict(HeadlineWordsLog, newdata=blenderData, type = "response")
blenderData$PredSvm = predict(HeadlineSvm, newdata=blenderData,probability = TRUE)
blenderData$Prednnet = predict (Headlinennet, newdata = blenderData, type = "raw")

testingData$PredRF = predict(HeadlineWordsRF, newdata=testingData, type = "prob")[,2]
testingData$Predgbm = predict(modelCasualglm, newdata=testingData, type = "prob")[,2]
testingData$PredLog = predict(HeadlineWordsLog, newdata=testingData, type = "response")
testingData$PredSvm = predict(HeadlineSvm, newdata=testingData,probability = TRUE)
testingData$Prednnet = predict (Headlinennet, newdata = testingData, type = "raw")

library(caret)
objControl = trainControl(method = 'cv',number = 5,classProbs= TRUE)

final_blender_model = train(as.factor(Popular) ~. ,data = blenderData,  method = "gbm",
                            trControl = objControl,
                            metric ="ROC",verbose = FALSE)

#final_blender_model = randomForest(as.factor(Popular)~., data = blenderData, importance = TRUE)
#varImpPlot(final_blender_model)
varImp (final_blender_model)

Prediction = predict(final_blender_model, newdata=testingData, type = "prob")[,2]


#library(caretEnsemble)
#objControl = trainControl(method = 'cv',number = 3,classProbs= TRUE)

#model_list = caretList (as.factor(Popular) ~. ,data = TrainHeadline,  methodList = c('gbm','glm','nnet','svmRadialWeights'),
#                                                    trControl = objControl,
#                                                    metric ="ROC",verbose = FALSE)


#Prediction = predict(model_list, newdata=TestHeadline, type = "prob")[,2]

MySubmissionCombined = data.frame(UniqueID = OriginalTest$UniqueID, Probability1 = Prediction)
colnames(MySubmissionCombined) = c("UniqueID","Probability1")
write.csv(MySubmissionCombined, "SubmissionHeadlineTrueEnsemble.csv", row.names=FALSE)
