#### Naive Bayes ####
library(e1071)
library(dplyr)
library(pROC)
library(caret)
insta.df <- read.csv("Instagram_fakeaccounts.csv", na.strings = "")
insta.df <- insta.df[-c(25,41,45),]

# factor 변환
insta.df$profile.pic <- factor(insta.df$profile.pic)
insta.df$name..username <- factor(insta.df$name..username)
insta.df$external.URL <- factor(insta.df$external.URL)
insta.df$private <- factor(insta.df$private)
insta.df$fake <- factor(insta.df$fake)

summary(insta.df)
str(insta.df)

# 구간 분할 위한 히스토그램
hist(insta.df$nums.length.username)
hist(insta.df$fullname.words)
hist(insta.df$nums.length.fullname)
hist(insta.df$description.length)
hist(insta.df$X.posts)
hist(insta.df$X.followers)
hist(insta.df$X.follows)

# 4분위 적용 -> X.posts , X.followers , X.follows
insta_quatile.df <- insta.df
insta_quatile.df[,c(9:11)] <- ntile(insta_quatile.df[,c(9:11)], 4)
insta_quatile.df$X.posts <- factor(insta_quatile.df$X.posts)
insta_quatile.df$X.followers <- factor(insta_quatile.df$X.followers)
insta_quatile.df$X.follows <- factor(insta_quatile.df$X.follows)

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

# 0을 따로 집계하는 25단위 7분할 -> description.length
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

# split data
set.seed(88)
train.index <- sample(c(1:dim(insta.df)[1]),dim(insta.df)[1]*0.7)

train_quatile_category_all.df <- insta_quatile_category.df[train.index,]
valid_quatile_category_all.df <- insta_quatile_category.df[-train.index,]

train_quatile_category_select.df <- insta_quatile_category.df[train.index,-c(3,4,5,7,8)]
valid_quatile_category_select.df <- insta_quatile_category.df[-train.index,-c(3,4,5,7,8)]

# run NaiveBayes
insta_all.nb <- naiveBayes(fake~., data=train_quatile_category_all.df)
pred_all.prob <- predict(insta_all.nb, newdata=valid_quatile_category_all.df, type="raw")
insta_all.nb

insta_select.nb <- naiveBayes(fake~., data=train_quatile_category_select.df)
pred_select.prob <- predict(insta_select.nb, newdata=valid_quatile_category_select.df, type="raw")
insta_select.nb

# ROC curve
ROC_insta_all <- roc(ifelse(valid_quatile_category_all.df$fake=="1",1,0), pred_all.prob[,2])
plot(ROC_insta_all, col="blue")

AUC_insta_all <- auc(ROC_insta_all)
AUC_insta_all

ROC_insta_select <- roc(ifelse(valid_quatile_category_select.df$fake=="1",1,0), pred_select.prob[,2])
plot(ROC_insta_select, col="blue")

AUC_insta_select <- auc(ROC_insta_select)
AUC_insta_select

# Confusion Matrix 비교 , all variables vs. selected variables
nb.pred.class.train_all <- predict(insta_all.nb, newdata=train_quatile_category_all.df)
confusionMatrix(nb.pred.class.train_all, train_quatile_category_all.df$fake, positive="1")

nb.pred.class.valid_all <- predict(insta_all.nb,newdata=valid_quatile_category_all.df)
confusionMatrix(nb.pred.class.valid_all, valid_quatile_category_all.df$fake, positive="1")

nb.pred.class.train_select <- predict(insta_select.nb, newdata=train_quatile_category_select.df)
confusionMatrix(nb.pred.class.train_select, train_quatile_category_select.df$fake, positive="1")

nb.pred.class.valid_select <- predict(insta_select.nb,newdata=valid_quatile_category_select.df)
confusionMatrix(nb.pred.class.valid_select, valid_quatile_category_select.df$fake, positive="1")
