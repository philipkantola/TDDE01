
#standard way of dividing set into train and test
n=dim(spambase)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=spambase[id,]
test=spambase[-id,] 


install.packages("kknn");
library(kknn)


# fits training data to a generalized linear model with binary output
model <- glm(formula = Spam ~ ., data=train, family=binomial)

# prints model
summary(model)

# creates vector and fills it with predicted values for the train set
model.trainprobs = predict(model, train, type ="response")
# creates vector and fills it with predicted values for the test set
model.probstestprobs = predict(model, test, type ="response")

summary(model.probs)

# confusion matrix p > 0.5, Spam is dependent variable, test on train
table(train$Spam, model.trainprobs > 0.5)
# confusion matrix p > 0.5, Spam is dependent variable, test on test
table(test$Spam, model.probstestprobs > 0.5)

# confusion matrix p > 0.8, test on train
table(train$Spam, model.trainprobs > 0.8)
# confusion matrix p > 0.8, test on test
table(test$Spam, model.probstestprobs > 0.8)

#Use standard classifier kknn() with K=30 from package kknn, report the the
#misclassification rates for the training and test data and compare the results with
#step 2.

#generate kknn (K=30) data for spam testing on train
spam.kknn30.train =  kknn(Spam ~ .,train=train, test=train, k=30)
#generate kknn (K=30) data for spam testing on test
spam.kknn30.test =  kknn(Spam ~ .,train=train, test=test, k=30)
# extract just the fitted values 
spam.kknn30.train.fit <- fitted(spam.kknn30.train)
spam.kknn30.test.fit <- fitted(spam.kknn30.test)

# print the confusion matrixes
table(train$Spam, spam.kknn30.train.fit > 0.5)
table(test$Spam, spam.kknn30.test.fit > 0.5)


# Repeat step 4 for K=1 

#generate kknn (K=1) data for spam testing on train
spam.kknn1.train =  kknn(Spam ~ .,train=train, test=train, k=1)
#generate kknn (K=1) data for spam testing on test
spam.kknn1.test =  kknn(Spam ~ .,train=train, test=test, k=1)
# extract just the fitted values 
spam.kknn1.train.fit <- fitted(spam.kknn1.train)
spam.kknn1.test.fit <- fitted(spam.kknn1.test)

# print the confusion matrixes
table(train$Spam, spam.kknn1.train.fit > 0.5)
table(test$Spam, spam.kknn1.test.fit > 0.5)



