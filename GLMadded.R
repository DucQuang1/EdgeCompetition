#Added new variables in the model 

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)


NewsTrainHeadline = read.csv("NewsTrainwHeadline.csv", stringsAsFactors=FALSE)
NewsTestHeadline = read.csv("NewsTestwHeadline.csv", stringsAsFactors=FALSE)

colnames(NewsTrainHeadline) = make.names(colnames(NewsTrainHeadline))
colnames(NewsTestHeadline) = make.names(colnames(NewsTestHeadline))

NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")


NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTest$Weekday = NewsTest$PubDate$wday

NewsTrain$Month = format(NewsTrain$PubDate,"%m")
NewsTest$Month = format(NewsTest$PubDate,"%m")

#Added back the variable
NewsTrainHeadline$Weekday = NewsTrain$Weekday
NewsTestHeadline$Weekday = NewsTest$Weekday
NewsTrainHeadline$Month = as.numeric(NewsTrain$Month) -9 
NewsTestHeadline$Month = as.numeric(NewsTest$Month) - 9



library(caret)
#NewsDeskNational and NewsDeskSports
NewsdeskTrain = as.data.frame(predict(dummyVars(" ~ NewsDesk",data = NewsTrain),newdata =NewsTrain))
NewsdeskTest = as.data.frame(predict(dummyVars(" ~ NewsDesk",data = NewsTest),newdata =NewsTest))

library(dplyr)
NewsdeskTrain = select(NewsdeskTrain, -(NewsDeskNational))
NewsdeskTrain = select(NewsdeskTrain,- (NewsDeskSports))

NewsTrainHeadline = cbind(NewsTrainHeadline,NewsdeskTrain)
NewsTestHeadline = cbind (NewsTestHeadline,NewsdeskTest)

SectionNameTrain = as.data.frame(predict(dummyVars(" ~ SectionName",data = NewsTrain),newdata =NewsTrain))
SectionNameTest = as.data.frame(predict(dummyVars(" ~ SectionName",data = NewsTest),newdata =NewsTest))

SectionNameTrain = select(SectionNameTrain, -SectionNameStyle)
SectionNameTrain = select(SectionNameTrain, -SectionNameSports)

NewsTrainHeadline = cbind(NewsTrainHeadline,SectionNameTrain)
NewsTestHeadline = cbind (NewsTestHeadline,SectionNameTest)
 
SubsectionTrain = as.data.frame(predict(dummyVars(" ~ SubsectionName",data = NewsTrain),newdata =NewsTrain))
SubsectionTest = as.data.frame(predict(dummyVars(" ~ SubsectionName",data = NewsTest),newdata =NewsTest))


colnames(SubsectionTrain) = make.names(colnames(SubsectionTrain))
colnames(SubsectionTest) = make.names(colnames(SubsectionTest))


SubsectionTrain = select(SubsectionTrain, -(SubsectionNameFashion...Style:SubsectionNamePolitics))



NewsTrainHeadline = cbind(NewsTrainHeadline,SubsectionTrain)
NewsTestHeadline = cbind (NewsTestHeadline,SubsectionTest)

colnames(NewsTrainHeadline) = make.names(colnames(NewsTrainHeadline))
colnames(NewsTestHeadline) = make.names(colnames(NewsTestHeadline))


library(randomForest)
HeadlineWordsRF = randomForest(factor(Popular) ~ ., data=NewsTrainHeadline)
PredTest = as.data.frame(predict(HeadlineWordsRF, newdata=NewsTestHeadline, type = "prob"))
MySubmissionRF = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest[,2])
write.csv(MySubmissionRF, "SubmissionHeadlineRF.csv", row.names=FALSE)


HeadlineWordsLog = glm (Popular~. , data = NewsTrainHeadline, family= binomial)
PredTest = predict(HeadlineWordsLog, newdata=NewsTestHeadline, type="response")
MySubmissionLog = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)
write.csv(MySubmissionLog, "SubmissionHeadlineLog.csv", row.names=FALSE)
