#standard way of dividing set into train and test
n=dim(tecator)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=tecator[id,]
test=tecator[-id,] 


# create a plot of Moisture versus Protein.
plot(tecator$Moisture, tecator$Protein)


# For each model, record the training and the validation MSE and
#present a plot showing how training and validation MSE depend on i
#(write some R code to make this plot). 

# poly creates a polynom which best fits the data in tecator$Protein with degree n
# created model out of train data
model1 <- lm(Moisture ~ poly(Protein, degree=1), data = train)
model2 <- lm(Moisture ~ poly(Protein, degree=2), data = train)
model3 <- lm(Moisture ~ poly(Protein, degree=3), data = train)
model4 <- lm(Moisture ~ poly(Protein, degree=4), data = train)
model5 <- lm(Moisture ~ poly(Protein, degree=5), data = train)
model6 <- lm(Moisture ~ poly(Protein, degree=6), data = train)

#making prediction for both test and train data for all six models
model1.prediction.train = predict(model1, train, type ="response")
model1.prediction.test = predict(model1, test, type ="response")
model2.prediction.train = predict(model2, train, type ="response")
model2.prediction.test = predict(model2, test, type ="response")
model3.prediction.train = predict(model3, train, type ="response")
model3.prediction.test = predict(model3, test, type ="response")
model4.prediction.train = predict(model4, train, type ="response")
model4.prediction.test = predict(model4, test, type ="response")
model5.prediction.train = predict(model5, train, type ="response")
model5.prediction.test = predict(model5, test, type ="response")
model6.prediction.train = predict(model6, train, type ="response")
model6.prediction.test = predict(model6, test, type ="response")

# calculating minimum square error for all predictions
# turns out the model is pretty bad,but the error increases on test with higher polynomial 
#due to higher overfitting
mse1.train <- mean((train$Moisture - model1.prediction.train)^2)
mse2.train <- mean((train$Moisture - model2.prediction.train)^2)
mse3.train <- mean((train$Moisture - model3.prediction.train)^2)
mse4.train <- mean((train$Moisture - model4.prediction.train)^2)
mse5.train <- mean((train$Moisture - model5.prediction.train)^2)
mse6.train <- mean((train$Moisture - model6.prediction.train)^2)
mse1.test <- mean((test$Moisture - model1.prediction.test)^2)
mse2.test <- mean((test$Moisture - model2.prediction.test)^2)
mse3.test <- mean((test$Moisture - model3.prediction.test)^2)
mse4.test <- mean((test$Moisture - model4.prediction.test)^2)
mse5.test <- mean((test$Moisture - model5.prediction.test)^2)
mse6.test <- mean((test$Moisture - model6.prediction.test)^2)

mse.test <- c(mse1.test,mse2.test,mse3.test,mse4.test,mse5.test,mse6.test)
mse.train <- c(mse1.train,mse2.train,mse3.train,mse4.train,mse5.train,mse6.train)


plot(mse.test,type="o", col="blue",ylim=c(10,60))
lines(mse.train, type="o",  col="red")

# Perform variable selection of a linear model in which Fat is response and 
#Channel1-Channel100 are predictors by using stepAIC

# package containing stepAIC function
library(MASS)

fat = tecator$Fat;

modelfat <- lm(fat ~ ., data = tecator[c(1:101)])

#stepAIC minimizes the features in use, by minimizing the AIC number, which is a kind of
#quantification of total information
# both = both backward and forward selection
stepAIC(modelfat, progress = "both")

# Fit a Ridge regression model with the same predictor and response variables. 
# Present a plot showing how model coefficients depend on the log of the penalty factor 𝜆
# and report how the coefficients change with 𝜆.

#library which contains functions for ridge and lasso
library(glmnet)

#makes matrix which is necessary for ridge
x <- model.matrix(fat~., -1, data = tecator[c(1:101)])

#fit linear model via penalized max likelihood, alpha = 0 gives ridge, alpha = 1 gives lasso
modelfat.ridge <- glmnet(x, fat, alpha=0)

# plot howcoefficients depend on log lambda , magic
plot(fit.ridge, xvar="lambda", label=TRUE)

# now fit lasso
modelfat.lasso <- glmnet(x, fat, alpha=1)
plot(modelfat.lasso, xvar="lambda", label=TRUE)

# Use cross-validation to find the optimal LASSO model (make sure that case λ = 0
# is also considered by the procedure) , report the optimal λ and how many
# variables were chosen by the model and make conclusions. Present also
# a plot showing the dependence of the CV score and comment how the CV score changes with λ.

# does k-fold crossvalidation  , default: k=10
cv.lasso <- cv.glmnet(x, fat, alpha=1, lambda=seq(0,1,0.001))
# graph to plot MSE and log lambda
plot(cv.lasso)

# lambda which minimizes MSE (0)
cv.lasso$lambda.min

help(cv.glmnet)
