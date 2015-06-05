#Added new variables in the model 

OriginalTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
OriginalTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)


TrainHeadline = read.csv("NewsTrainwHeadline.csv", stringsAsFactors=FALSE)
TestHeadline = read.csv("NewsTestwHeadline.csv", stringsAsFactors=FALSE)

colnames(OriginalTrain) = make.names(colnames(OriginalTrain))
colnames(OriginalTest) = make.names(colnames(OriginalTest))
colnames(TrainHeadline) = make.names(colnames(TrainHeadline))
colnames(TestHeadline) = make.names(colnames(TestHeadline))

OriginalTrain$PubDate = strptime(OriginalTrain$PubDate, "%Y-%m-%d %H:%M:%S")
OriginalTest$PubDate = strptime(OriginalTest$PubDate, "%Y-%m-%d %H:%M:%S")


OriginalTrain$Weekday = OriginalTrain$PubDate$wday
OriginalTest$Weekday = OriginalTest$PubDate$wday

OriginalTrain$Month = format(OriginalTrain$PubDate,"%m")
OriginalTest$Month = format(OriginalTest$PubDate,"%m")

#Added back the variable
TrainHeadline$Weekday = as.factor(OriginalTrain$Weekday)
TestHeadline$Weekday = as.factor(OriginalTest$Weekday)
TrainHeadline$Month = NULL
TestHeadline$Month = NULL 
TestHeadline$Popular = NULL

OriginalTrain$isTrain = 1
OriginalTest$isTrain = 0
OriginalTest$Popular = 0
fullset = rbind(OriginalTrain,OriginalTest)
fullset$NewsDesk = as.factor(fullset$NewsDesk)
fullset$SectionName = as.factor(fullset$SectionName)
fullset$SubsectionName = as.factor(fullset$SubsectionName)

trainset = fullset[fullset$isTrain == 1,]
testset = fullset[fullset$isTrain == 0,]


TrainHeadline$SectionName = trainset$SectionName
TestHeadline$SectionName = testset$SectionName


TrainHeadline$NewsDesk = trainset$NewsDesk
TestHeadline$NewsDesk = testset$NewsDesk

TrainHeadline$SubsectionName = trainset$SubsectionName
TestHeadline$SubsectionName = testset$SubsectionName

TrainHeadline$Popular[TrainHeadline$Popular == 1] = "Yes"
TrainHeadline$Popular[TrainHeadline$Popular == 0] = "No"



TrainHeadline$WordCount = log (OriginalTrain$WordCount +1)
TestHeadline$WordCount =  log (OriginalTest$WordCount +1)

#TrainHeadline$WordCount[TrainHeadline$WordCount == -Inf] = -0.5
#TestHeadline$WordCount[TestHeadline$WordCount == -Inf] = -0.5

library(caret)
objControl = trainControl(method = 'cv',number = 10,classProbs= TRUE)

modelCasualglm = train(as.factor(Popular) ~. ,data = TrainHeadline,  method = "gbm",
                       trControl = objControl,
                       metric ="ROC",verbose = FALSE)
Predgbm = as.data.frame(predict(modelCasualglm, newdata=TestHeadline, type = "prob"))


library(randomForest)
HeadlineWordsRF = randomForest(as.factor(Popular) ~ ., data= TrainHeadline)
PredRF = as.data.frame(predict(HeadlineWordsRF, newdata=TestHeadline, type = "prob"))

PredTest = (4*PredRF[,2] + 6*Predgbm[,2])/10

MySubmissionCombined = data.frame(UniqueID = OriginalTest$UniqueID, Probability1 = PredTest)
write.csv(MySubmissionRF, "SubmissionHeadlineCombined.csv", row.names=FALSE)








fitControl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)

library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
tr = train(as.factor(Popular) ~. ,data = TrainHeadline, method="rf", nodesize=5, ntree=500, metric="ROC", trControl=fitControl)

stopCluster(cl)


Predgbm = as.data.frame(predict(modelCasualglm, newdata=TestHeadline, type = "prob"))
MySubmissiongbm = data.frame(UniqueID = OriginalTest$UniqueID, Probability1 = Predgbm[,2])
write.csv(MySubmissiongbm, "SubmissionHeadlinegbm.csv", row.names=FALSE)

library(party)
HeadlineWordsCtree = ctree(Popular~., data = NewsTrainHeadline)
PredTest = predict(HeadlineWordsCtree, newdata = NewsTestHeadline)
MySubmissionCtree = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)
colnames(MySubmissionCtree) = c("UniqueID","Probability1")
write.csv(MySubmissionCtree, "SubmissionHeadlineCtree.csv", row.names=FALSE)

write.csv (NewsTestHeadline, "AddedNewsTest.csv", row.names = FALSE)
write.csv(NewsTrainHeadline, "AddedNewsTrain.csv", row.names = FALSE)








