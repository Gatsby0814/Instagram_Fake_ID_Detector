
#### kNN #### 종속변수만 factor로
library(FNN)
library(caret)
insta.df <- read.csv("Instagram_fakeaccounts.csv", na.strings = "")
insta.df <- insta.df[-c(25,41,45),]

insta.df$fake <- factor(insta.df$fake)

summary(insta.df)

#split data -> needs 3 datasets (6:2:2) , all variables vs. selected variables
set.seed(88)
spl <- sample(c(1:3), size=nrow(insta.df), replace = TRUE, prob=c(0.6,0.2,0.2))

train_all.df <- insta.df[spl==1,]
valid_all.df <- insta.df[spl==2,]
test_all.df <- insta.df[spl==3,]

train_select.df <- insta.df[spl==1,-c(4,5,7,8)]
valid_select.df <- insta.df[spl==2,-c(4,5,7,8)]
test_select.df <- insta.df[spl==3,-c(4,5,7,8)]

#Normalize -> "fullname.words" , "description.length" , "X.posts" , "X.followers" , "X.follows"
norm.values <- preProcess(insta.df[,c(3,6,9,10,11)], method=c("center","scale"))

train_all.norm.df <- train_all.df
valid_all.norm.df <- valid_all.df
test_all.norm.df <- test_all.df

train_all.norm.df[,c(3,6,9,10,11)] <- predict(norm.values, train_all.df[,c(3,6,9,10,11)])
valid_all.norm.df[,c(3,6,9,10,11)] <- predict(norm.values, valid_all.df[,c(3,6,9,10,11)])
test_all.norm.df[,c(3,6,9,10,11)] <- predict(norm.values, test_all.df[,c(3,6,9,10,11)])

train_select.norm.df <- train_select.df
valid_select.norm.df <- valid_select.df
test_select.norm.df <- test_select.df

train_select.norm.df[,c(3:7)] <- predict(norm.values, train_select.df[,c(3:7)])
valid_select.norm.df[,c(3:7)] <- predict(norm.values, valid_select.df[,c(3:7)])
test_select.norm.df[,c(3:7)] <- predict(norm.values, test_select.df[,c(3:7)])


#knn -> valid set에 적용하여 k값 찾기 (1 ~ 20 중에서), all variables vs. selected variables
accuracy_all.df <- data.frame(k=seq(1,20,1), accuracy=rep(0,20))
for(i in 1:20){
  knn_all.pred <- knn(train=train_all.norm.df[,1:11], test=valid_all.norm.df[,1:11], cl=train_all.norm.df[,12], k=i)
  accuracy_all.df[i,2] <- confusionMatrix(knn_all.pred, valid_all.norm.df[,12], positive="1")$overall[1]
}
accuracy_all.df

accuracy_select.df <- data.frame(k=seq(1,20,1), accuracy=rep(0,20))
for(i in 1:20){
  knn_select.pred <- knn(train=train_select.norm.df[,1:7], test=valid_select.norm.df[,1:7], cl=train_select.norm.df[,8], k=i)
  accuracy_select.df[i,2] <- confusionMatrix(knn_select.pred, valid_select.norm.df[,8], positive="1")$overall[1]
}
accuracy_select.df

#test set에 적용하여 confusion matrix 비교, all variables vs. selected variables
nn_valid_all <- knn(train=train_all.norm.df[,1:11], test=valid_all.norm.df[,1:11], cl=train_all.norm.df[,12], k=19)
confusionMatrix(nn_valid_all, valid_all.norm.df[,12], positive="1")

nn_test_all <- knn(train=train_all.norm.df[,1:11], test=test_all.norm.df[,1:11], cl=train_all.norm.df[,12], k=19)
confusionMatrix(nn_test_all, test_all.norm.df[,12], positive="1")

nn_valid_select <- knn(train=train_select.norm.df[,1:7], test=valid_select.norm.df[,1:7], cl=train_select.norm.df[,8], k=5)
confusionMatrix(nn_valid_select, valid_select.norm.df[,8], positive="1")

nn_test_select <- knn(train=train_select.norm.df[,1:7], test=test_select.norm.df[,1:7], cl=train_select.norm.df[,8], k=5)
confusionMatrix(nn_test_select, test_select.norm.df[,8], positive="1")
