#The read.csv and read.csv2 functions are nearly identical. 
#The only difference is that they are set up depending on whether you use 
#periods or commas as decimal points in numbers.
data1 <- read.csv2('NIRSpectra.csv')
set.seed(12345)

#1

# the feature space can not include the target which is to be predicted
data <- subset(data1, select = -c(Viscosity))
# function which returns PCA model
PCA <- prcomp(data)

#eigenvalues (sum of all squares)
lambda = PCA$sdev^2

# convert eigenvalues to %
# only one variable captures almost 95% of the variation
sprintf("%2.3f",lambda/sum(lambda)*100)
# this can also be seen in a screeplot
screeplot(PCA)
# according to these only two variables are necessery to capture more than 99% of variation 

# compare PC1 with PC2, variable 1 more important than variable 2. 
plot(PCA$x[,1], PCA$x[,2], xlab = "PC1", ylab = "PC2")

#2
U <- PCA$rotation
help(prcomp)

plot(U[,1], main="Traceplot, PC1")
plot(U[,2], main="Traceplot, PC2")


#3
set.seed(12345)

library(fastICA)
# ICA is a method to get ICA model
a <-fastICA(data, 2) 

# %*% for matrix multiplication
# luckily W and K can be obtained by the fastICA function
Wnew = a$K %*% a$W
# present columns of w' as trace plots
plot(Wnew[,1])
plot(Wnew[,2])
# Make a plot of the scores of the first two latent features
plot(a$S[,1], a$s[,2], xlab = "Latent feature 1", ylab = "Latent feature 2")

