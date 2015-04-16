#GLM model 

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)

NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

NewsTrainHeadline = read.csv("NewsTrainwHeadline.csv", stringsAsFactors=FALSE)
NewsTestHeadline = read.csv("NewsTestwHeadline.csv", stringsAsFactors=FALSE)

NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")

NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTest$Weekday = NewsTest$PubDate$wday

NewsTrain$Month = format(NewsTrain$PubDate,"%m")
NewsTest$Month = format(NewsTest$PubDate,"%m")

NewsTrainHeadline$Weekday = NewsTrain$Weekday
NewsTestHeadline$Weekday = NewsTest$Weekday

NewsTrainHeadline$Month = as.numeric(NewsTrain$Month) -9 
NewsTestHeadline$Month = as.numeric(NewsTest$Month) - 9

HeadlineWordsLog = glm(Popular ~ ., data=NewsTrainHeadline, family=binomial)

PredTest = predict(HeadlineWordsLog, newdata=NewsTestHeadline, type="response")


MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)

write.csv(MySubmission, "SubmissionHeadlineLog.csv", row.names=FALSE)


library(party)
HeadlineWordsLog = ctree(factor(Popular) ~ ., data=NewsTrainHeadline)
PredTest = predict(HeadlineWordsLog, newdata=NewsTestHeadline)
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)
colnames(MySubmission) = c("UniqueID","Probability1")
write.csv(MySubmission, "SubmissionHeadlineLog.csv", row.names=FALSE)
