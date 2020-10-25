#### RandomForest ####
library(randomForest)
library(dplyr)
library(caret)

insta.df <- read.csv("Instagram_fakeaccounts.csv", na.strings = "")
insta.df <- insta.df[-c(25,41,45),]

summary(insta.df)

insta.df$profile.pic <- factor(insta.df$profile.pic)
insta.df$name..username <- factor(insta.df$name..username)
insta.df$external.URL <- factor(insta.df$external.URL)
insta.df$private <- factor(insta.df$private)
insta.df$fake <- factor(insta.df$fake)


## 1. 연속형변수 그대로 적용한 RandomForest
set.seed(88)
train.index <- sample(c(1:dim(insta.df)[1]),dim(insta.df)[1]*0.7)
train.df <- insta.df[train.index,-c(3,5,7,8)]
valid.df <- insta.df[-train.index,-c(3,5,7,8)]

rf <- randomForest(fake~., data=train.df, ntree=500, importance=TRUE)
rf
varImpPlot(rf, type=1)

rf.pred.valid <- predict(rf, valid.df)
confusionMatrix(rf.pred.valid, valid.df$fake, positive="1")



## 2. 연속형변수를 범주화한 RandomForest (NaiveBayes에서 적용한 임의분류법)
# 4분위 적용 -> X.posts , X.followers , X.follows
insta_quatile.df <- insta.df
insta_quatile.df[,c(9:11)] <- ntile(insta_quatile.df[,c(9:11)], 4)
insta_quatile.df$X.posts <- factor(insta_quatile.df$X.posts)
insta_quatile.df$X.followers <- factor(insta_quatile.df$X.followers)
insta_quatile.df$X.follows <- factor(insta_quatile.df$X.follows)

summary(insta_quatile.df)

# 평균값으로 6분할 -> nums.length.username
insta_quatile_category.df <- insta_quatile.df
insta_quatile_category.df <- transform(insta_quatile_category.df, 
                                       nums.length.username = ifelse(nums.length.username < (mean(nums.length.username)), "0",
                                                                     ifelse(nums.length.username < (mean(nums.length.username)*2), "1",
                                                                            ifelse(nums.length.username < (mean(nums.length.username)*3), "2",
                                                                                   ifelse(nums.length.username < (mean(nums.length.username)*4), "3",
                                                                                          ifelse(nums.length.username < (mean(nums.length.username)*5), "4","5"
                                                                                          ))))))
insta_quatile_category.df$nums.length.username <- factor(insta_quatile_category.df$nums.length.username)

# 0 & 1 (0보다 크면) 의 2분할 -> nums.length.fullname
insta_quatile_category.df <- transform(insta_quatile_category.df,
                                       nums.length.fullname = ifelse(nums.length.fullname >0, "1","0"))

insta_quatile_category.df$nums.length.fullname <- factor(insta_quatile_category.df$nums.length.fullname)

# 0,1,2,3, 그리고 4이상으로 5분할 -> fullname words
insta_quatile_category.df <- transform(insta_quatile_category.df,
                                       fullname.words = ifelse(fullname.words >=4, "4",fullname.words))
insta_quatile_category.df$fullname.words <- factor(insta_quatile_category.df$fullname.words)

# 0을 따로 집계하는 25단위 6분할 -> description.length
insta_quatile_category.df <- transform(insta_quatile_category.df,
                                       description.length = ifelse(description.length == 0, "0",
                                                                   ifelse(description.length <= 25, "1",
                                                                          ifelse(description.length <= 50, "2",
                                                                                 ifelse(description.length <= 75, "3",
                                                                                        ifelse(description.length <= 100, "4",
                                                                                               ifelse(description.length <= 125, "5","6"
                                                                                               )))))))
insta_quatile_category.df$description.length <- factor(insta_quatile_category.df$description.length)

summary(insta_quatile_category.df)
str(insta_quatile_category.df)

# split data & RandomForest
#2-1. 4분위 적용
train_quatile.df <- insta_quatile.df[train.index,-c(3,5,7,8)]
valid_quatile.df <- insta_quatile.df[-train.index,-c(3,5,7,8)]

rf_quatile <- randomForest(fake~., data=train_quatile.df, ntree=500, importance=TRUE)
rf_quatile
varImpPlot(rf_quatile, type=1)

rf.pred.valid.quatile <- predict(rf_quatile, valid_quatile.df)
confusionMatrix(rf.pred.valid, valid.df$fake, positive="1")

#2-2. 4분위 + 전부 범주화 분류 적용
train_quatile_category.df <- insta_quatile_category.df[train.index,]
valid_quatile_category.df <- insta_quatile_category.df[-train.index,]

rf_quatile_category <- randomForest(fake~., data=train_quatile_category.df, ntree=500, importance=TRUE)
rf_quatile_category
varImpPlot(rf_quatile_category, type=1)

rf.pred.valid.quatile.category <- predict(rf_quatile_category, valid_quatile_category.df)
confusionMatrix(rf.pred.valid.quatile.category, valid.df$fake, positive="1")


##### 비교
varImpPlot(rf, type=1)
varImpPlot(rf_quatile, type=1)
varImpPlot(rf_quatile_category, type=1)

confusionMatrix(rf.pred.valid, valid.df$fake, positive="1")
confusionMatrix(rf.pred.valid.quatile, valid.df$fake, positive="1")
confusionMatrix(rf.pred.valid.quatile.category, valid.df$fake, positive="1")
