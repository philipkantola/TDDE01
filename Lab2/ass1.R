#1. Use australian-crabs.csv and make a scatterplot of carapace length (CL) 
#versus rear width (RW) where observations are colored by Sex. 
#Do you think that this data is easy to classify by linear discriminant analysis? 


crabsData <- read.csv('australian-crabs.csv')

set.seed(12345)
# plot RW vs CL and color after sex 
plot(crabsData$RW, crabsData$CL, col= crabsData$sex, xlab = "RW", ylab = "CL")

#2

# contains lda function
library(MASS)

# make lda analysis of sex with proportional prior
resLDA <- lda(sex ~ CL + RW, data = crabsData)
#get lda prediction, might have given better results if data was separated to train and test
resLDA.predict <- predict(resLDA, crabsData)
# plot RW vs CL and color after predicted sex
plot(crabsData$RW, crabsData$CL, col= resLDA.predict$class, xlab = "RW", ylab = "CL")
#confusion matrix which gives misclassification rate
table(resLDA.predict$class, crabsData$sex)

#3
# make lda analysis of sex with prior probability 0.9 towards men
resLDA2 <- lda(sex ~ CL + RW, data = crabsData, prior=c(0.9,0.1))
print(resLDA2)
resLDA2.predict <- predict(resLDA2, crabsData)
plot(crabsData$RW, crabsData$CL, col= resLDA2.predict$class, xlab = "RW", ylab = "CL")
table(resLDA2.predict$class, crabsData$sex)

#4

# use logistic regression to classify instead of lda
resLR <- glm(formula = sex ~ RW + CL, data = crabsData, family = binomial)
resLR.predict <- predict(resLR, crabsData)
# glm classifies in a non-binary way, (ex -2,3 or 5), this creates a binary vector instead
isMale <- ifelse(resLR.predict > 0, 2,1)

plot(crabsData$RW, crabsData$CL, col= isMale, xlab = "RW", ylab = "CL")
# missclassification rate
table(crabsData$sex, resLR.predict > 0.5)

# get slope and intercept of the decision boundary
intercept <- -coef(resLR)[1]/(coef(resLR)[3])
slope <- coef(resLR)[2]/(-coef(resLR)[3])

plot(crabsData$RW, crabsData$CL, col= isMale, xlab = "RW", ylab = "CL")
# add line to plot from intercept and slope
abline(a = intercept, b= slope, col="GREEN")

