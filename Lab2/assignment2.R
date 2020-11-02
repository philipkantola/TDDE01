
RNGversion('3.5.1')

#Import the data to R and divide into training/validation/test as 50/25/25:
library(gdata)
creditscoring = read.xls('creditscoring.xls') 
n=dim(creditscoring)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=creditscoring[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
valid=creditscoring[id2,]
id3=setdiff(id1,id2)
test=creditscoring[id3,]

library(tree)

#2
# fit trees to data with impurity measures deviance and gini
deviance.tree = tree(as.factor(good_bad) ~ ., data = train, split = "deviance")
gini.tree = tree(as.factor(good_bad) ~ ., data = train, split = "gini")

deviance.predict.train=predict(deviance.tree, newdata=train, type="class")
deviance.predict.test=predict(deviance.tree, newdata=test, type="class")
gini.predict.train=predict(gini.tree, newdata=train, type="class")
gini.predict.test=predict(gini.tree, newdata=test, type="class")

# use mean to get misclassification error, can also use confusion table to calculate by hand
deviance.error.train = mean(deviance.predict.train != train$good_bad)
deviance.error.test = mean(deviance.predict.test != test$good_bad)
gini.error.train = mean(gini.predict.train != train$good_bad)
gini.error.test = mean(gini.predict.test != test$good_bad)

#3
#selecting optimal tree by train/validation
trainScore=rep(0,9)
testScore=rep(0,9)
# i is number of leaves
for(i in 2:9) {
  prunedTree=prune.tree(deviance.tree,best=i)
  pred=predict(prunedTree, newdata=valid, type="tree")
  trainScore[i]=deviance(prunedTree)
  testScore[i]=deviance(pred)
}

help(deviance)
# plot the dependence of deviances for data on the number of leaves.
plot(2:9, trainScore[2:9], type = "b", col="red", ylim=c(250,750))
points(2:9, testScore[2:9], type="b", col="blue")

# missclassification rate for optimal tree on test data 
prunedTree=prune.tree(deviance.tree,best=4)
# variables used
summary(prunedTree)
# structure of tree
plot(prunedTree)
text(prunedTree)
# type must be class to be able to compare with test$good_bads
pred=predict(prunedTree, newdata=test, type="class")
prunedTree.error.Test = mean(pred != test$good_bad)

#4
# library to get naiveBayes
library(e1071)
# fit a model based on naiveBayes 
naive.model  =naiveBayes(as.factor(good_bad)~., data=train)
naive.predict.train = predict(naive.model, newdata=train) 
summary(naive.predict.train)
naive.predict.test = predict(naive.model, newdata=test)
naive.predict.train[,1]

# confusion matrices
table(naive.predict.train, train$good_bad)
table(naive.predict.test, test$good_bad)

# missclassification rates
naive.error.train = mean(naive.predict.train != train$good_bad)
naive.error.test = mean(naive.predict.test != test$good_bad)

#5
# TPR = true positive rates = TP / Ptot, FPR = false positive rates = FP / Ntot

# type = raw gives probability
naive.predict.test = predict(naive.model, newdata = test, type = "raw")
# type = vector gives probability 
prunedTree.predict = predict(prunedTree, newdata = test, type = "vector" )
prunedTree.predict

# create vector with threshold values
pi = seq(0.05, 0.95, 0.05)

# creates vector of length pi with only zeros 
naive.TPR = 1:length(pi)
naive.FPR = 1:length(pi)
prunedTree.TPR = 1:length(pi)
prunedTree.FPR = 1:length(pi)

#library which contains FPR and TPR
library(measures)

for (i in 1:length(pi)){
  
  # if predicted good is above threshold label as 'good' otherwise 'bad'
  naive.temp = ifelse(naive.predict.test[,2] > pi[i], "good", "bad")
  
  # make a matrix out of confusion matrix
  naive.matrix = as.matrix(table(as.factor(naive.temp), test$good_bad))
  naive.TPR[i] = naive.matrix[2,2]/(naive.matrix[1,2]+naive.matrix[2,2])
  naive.FPR[i] = naive.matrix[2,1]/(naive.matrix[2,1]+naive.matrix[1,1])
  
  prunedTree.temp = ifelse(prunedTree.predict[,2] > pi[i], "good", "bad")
  t = table(prunedTree.temp,test$good_bad)
  # sometimes no good or no bad is predicted, if so we have to skip 
  if(nrow(t)>1){
  prunedTree.matrix = as.matrix(table(as.factor(prunedTree.temp), test$good_bad))
  prunedTree.TPR[i] <- prunedTree.matrix[2,2]/(prunedTree.matrix[1,2]+prunedTree.matrix[2,2])
  prunedTree.FPR[i] <- prunedTree.matrix[2,1]/(prunedTree.matrix[2,1]+prunedTree.matrix[1,1])
  }
  
}


plot(c(1,naive.FPR,0), c(1,naive.TPR,0), xlim=c(0, 1), ylim=c(0, 1), col = "blue", type="b", xlab = "FPR", ylab = "TPR" )
#at first the points were connected with other points(?) outside the graph, solved by excluding points
# with FPR higher than 2
points(c(prunedTree.FPR[prunedTree.FPR<=1],0),c(prunedTree.TPR[prunedTree.TPR<=1],0), col = "red", type="b")
legend("bottomright", lty = c(1, 1, 1), col = c("red", "blue"), legend = c("Tree", "Bayes"))

#6

# how to get confusion matrix with loss taken into account
naive.model  =naiveBayes(as.factor(good_bad)~., data=train)
naive.predict.train = predict(naive.model, newdata=train, type = "raw") 
naive.predict.test = predict(naive.model, newdata=test, type = "raw") 
# naive.predict.train[,1] is a vector of probabilities that each observation is "bad" 
# naive.predict.train[,2] is a vector of probabilities that each observation is "good" 
# if threshold (ratio) is higher than 10, rated as TRUE/good
naive.loss.train.conf <- table(train$good_bad, naive.predict.train[,2]/naive.predict.train[,1] > 10)
naive.loss.test.conf <- table(test$good_bad, naive.predict.test[,2]/naive.predict.test[,1] > 10)



