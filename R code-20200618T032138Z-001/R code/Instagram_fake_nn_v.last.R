insta.df <- read.csv("Instagram_fakeaccounts.csv", na.strings = "")
dim(insta.df)
summary(insta.df)
View(insta.df)

# insta.df2 == excluded_Outliers
insta.df2 <- insta.df[-c(25,41,45),]
dim(insta.df2)
summary(insta.df2)
View(insta.df2)



library(nnet)
library(neuralnet)


# transfer from conituous num to 0~1 (including outliers)
insta.df$fullname.words = (insta.df$fullname.words - min(insta.df$fullname.words)) / (max(insta.df$fullname.words) - min(insta.df$fullname.words))
insta.df$description.length = (insta.df$description.length - min(insta.df$description.length)) / (max(insta.df$description.length) - min(insta.df$description.length))
insta.df$X.posts = (insta.df$X.posts - min(insta.df$X.posts)) / (max(insta.df$X.posts) - min(insta.df$X.posts))
insta.df$X.followers =(insta.df$X.followers - min(insta.df$X.followers)) / (max(insta.df$X.followers) - min(insta.df$X.followers))
insta.df$X.follows = (insta.df$X.follows - min(insta.df$X.follows)) / (max(insta.df$X.follows) - min(insta.df$X.follows))
class.df <- class.ind(insta.df$fake)
colnames(class.df) = c(paste("fake_", c(0,1), sep=""))
head(class.df)

insta.df <- cbind(insta.df, class.df)



# transfer from conituous num to 0~1 (excluding outliers )
insta.df2$fullname.words = (insta.df2$fullname.words - min(insta.df2$fullname.words)) / (max(insta.df2$fullname.words) - min(insta.df2$fullname.words))
insta.df2$description.length = (insta.df2$description.length - min(insta.df2$description.length)) / (max(insta.df2$description.length) - min(insta.df2$description.length))
insta.df2$X.posts = (insta.df2$X.posts - min(insta.df2$X.posts)) / (max(insta.df2$X.posts) - min(insta.df2$X.posts))
insta.df2$X.followers =(insta.df2$X.followers - min(insta.df2$X.followers)) / (max(insta.df2$X.followers) - min(insta.df2$X.followers))
insta.df2$X.follows = (insta.df2$X.follows - min(insta.df2$X.follows)) / (max(insta.df2$X.follows) - min(insta.df2$X.follows))
class.df2 <- class.ind(insta.df2$fake)
colnames(class.df2) = c(paste("fake_", c(0,1), sep=""))
head(class.df2)

insta.df2 <- cbind(insta.df2, class.df2)


# set seed and seperating the data into train, valid
set.seed(88)
train.index <- sample(c(1:dim(insta.df)[1]), dim(insta.df)[1]*0.7)
valid.index <- setdiff(row.names(insta.df), train.index)
train.df <- insta.df[train.index,]
valid.df <- insta.df[-train.index,]


train.index2 <- sample(c(1:dim(insta.df2)[1]), dim(insta.df2)[1]*0.7)
valid.index2 <- setdiff(row.names(insta.df2), train.index2)
train.df2 <- insta.df2[train.index2,]
valid.df2 <- insta.df2[-train.index2,]

# modeling neural network _1: c(2,2) _2: c(3,2)
nn1_1 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + fullname.words + nums.length.fullname + name..username + description.length + external.URL + private + X.posts + X.follows, data = train.df, hidden = c(2,2))
nn1_2 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username 
                   + fullname.words + nums.length.fullname + name..username + description.length 
                   + external.URL + private + X.posts + X.follows, data = train.df, hidden = c(3,2))
plot(nn_1)
plot(nn_2)


nn2_1 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + fullname.words + nums.length.fullname + name..username + description.length + external.URL + private + X.posts + X.follows, data = train.df2, hidden = c(2,2))
nn2_2 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + fullname.words + nums.length.fullname + name..username + description.length + external.URL + private + X.posts + X.follows, data = train.df2, hidden = c(3,2))
plot(nn2_1)
plot(nn2_2)




# selection variables 10~5
nn1_1_10 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + fullname.words + nums.length.fullname + name..username + description.length + external.URL + X.posts + X.follows, data = train.df, hidden = c(2,2))
nn1_1_9 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + fullname.words + nums.length.fullname + description.length + external.URL + X.posts + X.follows, data = train.df, hidden = c(2,2))
nn1_1_8 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + fullname.words + nums.length.fullname + description.length + X.posts + X.follows, data = train.df, hidden = c(2,2))
nn1_1_7 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + fullname.words + description.length + X.posts + X.follows, data = train.df, hidden = c(2,2))
nn1_1_6 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + description.length + X.posts + X.follows, data = train.df, hidden = c(2,2))
nn1_1_5 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + description.length + X.posts, data = train.df, hidden = c(2,2))


nn1_2_10 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + fullname.words + nums.length.fullname + name..username + description.length + external.URL + X.posts + X.follows, data = train.df, hidden = c(3,2))
nn1_2_9 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + fullname.words + nums.length.fullname + description.length + external.URL + X.posts + X.follows, data = train.df, hidden = c(3,2))

nn1_2_8 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + fullname.words + nums.length.fullname + description.length + X.posts + X.follows, data = train.df, hidden = c(3,2))

nn1_2_7 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username +
                       fullname.words + description.length + X.posts 
                     + X.follows, data = train.df, hidden = c(3,2))

nn1_2_6 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + description.length + X.posts + X.follows, data = train.df, hidden = c(3,2))
nn1_2_5 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + description.length + X.posts, data = train.df, hidden = c(3,2))


nn2_1_10 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + fullname.words + nums.length.fullname + name..username + description.length + external.URL + X.posts + X.follows, data = train.df2, hidden = c(2,2))
nn2_1_9 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + fullname.words + nums.length.fullname + description.length + external.URL + X.posts + X.follows, data = train.df2, hidden = c(2,2))
nn2_1_8 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + fullname.words + nums.length.fullname + description.length + X.posts + X.follows, data = train.df2, hidden = c(2,2))
nn2_1_7 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + fullname.words + description.length + X.posts + X.follows, data = train.df2, hidden = c(2,2))
nn2_1_6 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + description.length + X.posts + X.follows, data = train.df2, hidden = c(2,2))
nn2_1_5 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + description.length + X.posts, data = train.df2, hidden = c(2,2))


nn2_2_10 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + fullname.words + nums.length.fullname + name..username + description.length + external.URL + X.posts + X.follows, data = train.df2, hidden = c(3,2))
nn2_2_9 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + fullname.words + nums.length.fullname + description.length + external.URL + X.posts + X.follows, data = train.df2, hidden = c(3,2))
nn2_2_8 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + fullname.words + nums.length.fullname + description.length + X.posts + X.follows, data = train.df2, hidden = c(3,2))
nn2_2_7 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + fullname.words + description.length + X.posts + X.follows, data = train.df2, hidden = c(3,2))
nn2_2_6 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + description.length + X.posts + X.follows, data = train.df2, hidden = c(3,2))
nn2_2_5 <- neuralnet(fake_0 +  fake_1 ~ profile.pic + nums.length.username + description.length + X.posts, data = train.df2, hidden = c(3,2))



# classification of prediction according to hiddend layers and outliers (train)
training.prediction1_1 <- compute(nn1_1, train.df[,-c(12:14)])
training.class1_1 <- apply(training.prediction1_1$net.result,1,which.max)-1
head(training.class1_1)

training.prediction1_2 <- compute(nn1_2, train.df[,-c(12:14)])
training.class1_2 <- apply(training.prediction1_2$net.result,1,which.max)-1
head(training.class1_2)

training.prediction2_1 <- compute(nn2_1, train.df2[,-c(12:14)])
training.class2_1 <- apply(training.prediction2_1$net.result,1,which.max)-1
head(training.class2_1)

training.prediction2_2 <- compute(nn2_2, train.df2[,-c(12:14)])
training.class2_2 <- apply(training.prediction2_2$net.result,1,which.max)-1
head(training.class2_2)


# prediction according to hidden layers, outliers and variables
training.prediction1_1_10 <- compute(nn1_1_10, train.df[,-c(12:14)])
training.class1_1_10 <- apply(training.prediction1_1_10$net.result,1,which.max)-1
head(training.class1_1_10)

training.prediction1_1_9 <- compute(nn1_1_9, train.df[,-c(12:14)])
training.class1_1_9 <- apply(training.prediction1_1_9$net.result,1,which.max)-1
head(training.class1_1_9)

training.prediction1_1_8 <- compute(nn1_1_8, train.df[,-c(12:14)])
training.class1_1_8 <- apply(training.prediction1_1_8$net.result,1,which.max)-1
head(training.class1_1_8)

training.prediction1_1_7 <- compute(nn1_1_7, train.df[,-c(12:14)])
training.class1_1_7 <- apply(training.prediction1_1_7$net.result,1,which.max)-1
head(training.class1_1_7)

training.prediction1_1_6 <- compute(nn1_1_6, train.df[,-c(12:14)])
training.class1_1_6 <- apply(training.prediction1_1_6$net.result,1,which.max)-1
head(training.class1_1_6)

training.prediction1_1_5 <- compute(nn1_1_5, train.df[,-c(12:14)])
training.class1_1_5 <- apply(training.prediction1_1_5$net.result,1,which.max)-1
head(training.class1_1_5)

training.prediction1_2_10 <- compute(nn1_2_10, train.df[,-c(12:14)])
training.class1_2_10 <- apply(training.prediction1_2_10$net.result,1,which.max)-1
head(training.class1_2_10)

training.prediction1_2_9 <- compute(nn1_2_9, train.df[,-c(12:14)])
training.class1_2_9 <- apply(training.prediction1_2_9$net.result,1,which.max)-1
head(training.class1_2_9)

training.prediction1_2_8 <- compute(nn1_2_8, train.df[,-c(12:14)])
training.class1_2_8 <- apply(training.prediction1_2_8$net.result,1,which.max)-1
head(training.class1_2_8)

training.prediction1_2_7 <- compute(nn1_2_7, train.df[,-c(12:14)])
training.class1_2_7 <- apply(training.prediction1_2_7$net.result,1,which.max)-1
head(training.class1_2_7)

training.prediction1_2_6 <- compute(nn1_2_6, train.df[,-c(12:14)])
training.class1_2_6 <- apply(training.prediction1_2_6$net.result,1,which.max)-1
head(training.class1_2_6)

training.prediction1_2_5 <- compute(nn1_2_5, train.df[,-c(12:14)])
training.class1_2_5 <- apply(training.prediction1_2_5$net.result,1,which.max)-1
head(training.class1_2_5)


training.prediction2_1_10 <- compute(nn2_1_10, train.df2[,-c(12:14)])
training.class2_1_10 <- apply(training.prediction2_1_10$net.result,1,which.max)-1
head(training.class2_1_10)

training.prediction2_1_9 <- compute(nn2_1_9, train.df2[,-c(12:14)])
training.class2_1_9 <- apply(training.prediction2_1_9$net.result,1,which.max)-1
head(training.class2_1_9)

training.prediction2_1_8 <- compute(nn2_1_8, train.df2[,-c(12:14)])
training.class2_1_8 <- apply(training.prediction2_1_8$net.result,1,which.max)-1
head(training.class2_1_8)

training.prediction2_1_7 <- compute(nn2_1_7, train.df2[,-c(12:14)])
training.class2_1_7 <- apply(training.prediction2_1_7$net.result,1,which.max)-1
head(training.class2_1_7)

training.prediction2_1_6 <- compute(nn2_1_6, train.df2[,-c(12:14)])
training.class2_1_6 <- apply(training.prediction2_1_6$net.result,1,which.max)-1
head(training.class2_1_6)

training.prediction2_1_5 <- compute(nn2_1_5, train.df2[,-c(12:14)])
training.class2_1_5 <- apply(training.prediction2_1_5$net.result,1,which.max)-1
head(training.class2_1_5)

training.prediction2_2_10 <- compute(nn2_2_10, train.df2[,-c(12:14)])
training.class2_2_10 <- apply(training.prediction2_2_10$net.result,1,which.max)-1
head(training.class2_2_10)

training.prediction2_2_9 <- compute(nn2_2_9, train.df2[,-c(12:14)])
training.class2_2_9 <- apply(training.prediction2_2_9$net.result,1,which.max)-1
head(training.class2_2_9)

training.prediction2_2_8 <- compute(nn2_2_8, train.df2[,-c(12:14)])
training.class2_2_8 <- apply(training.prediction2_2_8$net.result,1,which.max)-1
head(training.class2_2_8)

training.prediction2_2_7 <- compute(nn2_2_7, train.df2[,-c(12:14)])
training.class2_2_7 <- apply(training.prediction2_2_7$net.result,1,which.max)-1
head(training.class2_2_7)

training.prediction2_2_6 <- compute(nn2_2_6, train.df2[,-c(12:14)])
training.class2_2_6 <- apply(training.prediction2_2_6$net.result,1,which.max)-1
head(training.class2_2_6)

training.prediction2_2_5 <- compute(nn2_2_5, train.df2[,-c(12:14)])
training.class2_2_5 <- apply(training.prediction2_2_5$net.result,1,which.max)-1
head(training.class2_2_5)


# Confusion Matrix
library(caret)

# CM hidden layer (2,2)/ outlier
confusionMatrix(factor(training.class1_1), factor(train.df$fake), positive="1")
confusionMatrix(factor(training.class1_2), factor(train.df$fake), positive="1")


confusionMatrix(factor(training.class2_1), factor(train.df2$fake), positive="1")
confusionMatrix(factor(training.class2_2), factor(train.df2$fake), positive="1")


# CM  including outlier/ hidden layer (2,2)/ var 10~5 
confusionMatrix(factor(training.class1_1_10), factor(train.df$fake), positive="1")
confusionMatrix(factor(training.class1_1_9), factor(train.df$fake), positive="1")
confusionMatrix(factor(training.class1_1_8), factor(train.df$fake), positive="1")
confusionMatrix(factor(training.class1_1_7), factor(train.df$fake), positive="1")
confusionMatrix(factor(training.class1_1_6), factor(train.df$fake), positive="1")
confusionMatrix(factor(training.class1_1_5), factor(train.df$fake), positive="1")

# CM  including outlier/ hidden layer (2,3)/ var 10~5 
confusionMatrix(factor(training.class1_2_10), factor(train.df$fake), positive="1")
confusionMatrix(factor(training.class1_2_9), factor(train.df$fake), positive="1")
confusionMatrix(factor(training.class1_2_8), factor(train.df$fake), positive="1")
confusionMatrix(factor(training.class1_2_7), factor(train.df$fake), positive="1")
confusionMatrix(factor(training.class1_2_6), factor(train.df$fake), positive="1")
confusionMatrix(factor(training.class1_2_5), factor(train.df$fake), positive="1")

# CM  excluding outlier/ hidden layer (2,2)/ var 10~5 
confusionMatrix(factor(training.class2_1_10), factor(train.df2$fake), positive="1")
confusionMatrix(factor(training.class2_1_9), factor(train.df2$fake), positive="1")
confusionMatrix(factor(training.class2_1_8), factor(train.df2$fake), positive="1")
confusionMatrix(factor(training.class2_1_7), factor(train.df2$fake), positive="1")
confusionMatrix(factor(training.class2_1_6), factor(train.df2$fake), positive="1")
confusionMatrix(factor(training.class2_1_5), factor(train.df2$fake), positive="1")

# CM  excluding outlier/ hidden layer (3,2)/ var 10~5 
confusionMatrix(factor(training.class2_2_10), factor(train.df2$fake), positive="1")
confusionMatrix(factor(training.class2_2_9), factor(train.df2$fake), positive="1")
confusionMatrix(factor(training.class2_2_8), factor(train.df2$fake), positive="1")
confusionMatrix(factor(training.class2_2_7), factor(train.df2$fake), positive="1")
confusionMatrix(factor(training.class2_2_6), factor(train.df2$fake), positive="1")
confusionMatrix(factor(training.class2_2_5), factor(train.df2$fake), positive="1")



# selecting model and mining valid
# classification of predictions (valid)
valid.prediction1_2 <- compute(nn1_2, valid.df[,-c(12:14)])
valid.prediction1_2$net.result
valid.class1_2 <- apply(valid.prediction1_2$net.result,1,which.max)-1


valid.prediction1_2_7 <- compute(nn1_2_7, valid.df[,-c(12:14)])
valid.prediction1_2_7$net.result
valid.class1_2_7 <- apply(valid.prediction1_2_7$net.result,1,which.max)-1

valid.prediction1_2_10 <- compute(nn1_2_10, valid.df[,-c(12:14)])
valid.prediction1_2_10$net.result
valid.class1_2_10 <- apply(valid.prediction1_2_10$net.result,1,which.max)-1

valid.prediction1_1_7 <- compute(nn1_1_7, valid.df[,-c(12:14)])
valid.prediction1_1_7$net.result
valid.class1_1_7 <- apply(valid.prediction1_1_7$net.result,1,which.max)-1


#Valid ConfusionMatrix
confusionMatrix(factor(valid.class1_2), factor(valid.df$fake), positive="1")
confusionMatrix(factor(valid.class1_2_7), factor(valid.df$fake), positive="1")
confusionMatrix(factor(valid.class1_2_10), factor(valid.df$fake), positive="1")
confusionMatrix(factor(valid.class1_1_7), factor(valid.df$fake), positive="1")
