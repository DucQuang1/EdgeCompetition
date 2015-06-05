#Exploratory data analysis 
str(TrainHeadline)
prop.table(table(TrainHeadline$Popular, TrainHeadline$today),2)
prop.table(table(TrainHeadline$Popular, TrainHeadline$york),2)
prop.table(table(TrainHeadline$Popular, TrainHeadline$springsumm),2)
prop.table(table(TrainHeadline$Popular, TrainHeadline$SectionName),2)
prop.table(table(TrainHeadline$Popular, TrainHeadline$china),2)



library(ggplot2)
qplot(SectionName, data = TrainHeadline, color = Popular)
qplot(Weekday, data = TrainHeadline, color = Popular)
qplot(SubsectionName , data= TrainHeadline, color = Popular)
qplot(NewsDesk, data = TrainHeadline, color = Popular)
qplot (WordCount, data = TrainHeadline, color = Popular)
summary(TrainHeadline$WordCount)

library(randomForest)
varImpPlot (HeadlineWordsRF)
library(caret)
varImp (modelCasualglm)

table(TrainHeadline$NewsDesk, TrainHeadline$SectionName)
#table(TrainHeadline$NewsDesk, TrainHeadline$SubsectionName)
#table(TrainHeadline$SectionName,TrainHeadline$SubsectionName)

prop.table(table(TrainHeadline$SectionName))
#SubsectionName has a lot of missing 70%
prop.table(table(TrainHeadline$SubsectionName))
prop.table(table(TrainHeadline$NewsDesk))

str(OriginalTrain)