# neuralnet package requires numeric inputs and does not play nicely with factor variables
library(neuralnet)
set.seed(1234567890)
# generate 50 random values (uniformly distributed) between 0 and 10. 
Variables <- runif(50, 0, 10)
trva <- data.frame(Variables, Sin=sin(Variables))
train <- trva[1:25,] # Training
validate <- trva[26:50,] # Validation

# Random initialization of the weights in the interval [-1, 1]
# 31 weights, one for every  vertex (båg?)
winit <- runif(31,-1,1)

error <- 1:10
# Train model with different thresholds
for(i in 1:10) {

  nn <- neuralnet(Sin ~ Variables, threshold = i/1000, data = train , hidden = 10, startweights = winit)
  prediction <- predict(nn, validate)
  error[i] <- mean(sqrt((validate$Sin-prediction)^2))

}
# returns smallest value of a vector (7/1000)
best = which.min(abs(error))
plot(error,type="o")


# final neuralnet, the best one
nn <- neuralnet(Sin ~ Variables, threshold = best/1000, data = train , hidden = 10, startweights = winit)
prediction <- predict(nn, validate)

plot(validate$Variables, prediction,  ylab = "Sin", xlab = "Value")
points(validate$Variables, validate$Sin, col = "red")

