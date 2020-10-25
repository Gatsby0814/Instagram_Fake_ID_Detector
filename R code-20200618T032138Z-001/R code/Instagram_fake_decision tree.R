insta.df <- read.csv("2020datamining/Instagram_fakeaccounts.csv", na.strings = "")

library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(e1071)

#º¯¼ö»«¹öÀü
insta.df$profile.pic <- factor(insta.df$profile.pic)
insta.df$name..username <- factor(insta.df$name..username)
insta.df$external.URL <- factor(insta.df$external.URL)
insta.df$private <- factor(insta.df$private)
insta.df$fake <- factor(insta.df$fake)

set.seed(88)
train.index <- sample(c(1:dim(insta.df)[1]),dim(insta.df)[1]*0.7)
train.df <- insta.df[train.index,c(1,2,6,9,10,12)]
valid.df <- insta.df[-train.index,c(1,2,6,9,10,12)]
View(train.df)

#decision tree
default.ct <- rpart(fake~.,data=train.df,method="class")
prp(default.ct,type=1,extra=1, under=TRUE,split.font = 1, varlen=-10,box.col=ifelse(default.ct$frame$var =="<leaf>",'gray','white'))
#train.df overfit check
default.ct.pred.train <- predict(default.ct,train.df,type="class")
confusionMatrix(default.ct.pred.train,train.df$fake)
#valid.df confusionmatrix
default.ct.pred.valid <- predict(default.ct,valid.df,type="class")
confusionMatrix(default.ct.pred.valid,valid.df$fake)
