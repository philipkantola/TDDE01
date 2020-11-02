# Assume the probability model p(x|θ) = θ*exp(-θx) for x=Length in which
#observations are independent and identically distributed. What is the
#distribution type of x? Write a function that computes the log-likelihood
#log p(x|θ)for a given 𝜃 and a given data vector x. Plot the curve showing the
#dependence of log-likelihood on 𝜃 where the entire data is used for fitting.
#What is the maximum likelihood value of t� according to the plot?

install.packages("rmarkdown")

set.seed(12345)

# plots the data 
plot(machines)

# log(θ*exp(-θx)) = log(θ) -θx
# loglikelihood = sum of log of fx for every observed x with θ as variable  
# function that computes the log-likelihood 
loglikelihood <- function(theta,X) {
  return(dim(X)[1]*log(theta)-theta*sum(X))
}

#Plot the curve showing the dependence of log-likelihood on θ of the entire data
curve(dim(machines)[1]*log(x)-x*sum(machines), 0, 4, xlab="θ", ylab="log(p(x|θ))")

# using optimize to get the maximum of the function 
optimize(loglikelihood, X=machines, c(0, 5), maximum= TRUE)$maximum

#Repeat step 2 but use only 6 first observations from the data, and put the two
#log-likelihood curves (from step 2 and 3) in the same plot. What can you say
#about reliability of the maximum likelihood solution in each case?

# creating a subset of the machine length vector
machines.sub <- matrix(machines[1:6,1], nrow = 6, ncol = 1)

#Plot the curve showing the dependence of log-likelihood on θ of data point 1-6, comparing with whole data set
curve(dim(machines.sub)[1]*log(x)-x*sum(machines.sub), 0, 25, xlab="θ", ylab="log(p(x|θ))", col="blue")
curve(dim(machines)[1]*log(x)-x*sum(machines), 0, 25, xlab="θ", ylab="log(p(x|θ))", col="red", add=TRUE)

# using optimize to get the maximum of the function 
optimize(loglikelihood, X=machines.sub, c(0, 5), maximum= TRUE)$maximum

#The reliability for the second data set is lower, since it is based on less data points. Therefore the curve is much wider, 
#we can’t make a prediction as specific as for the first data set which has a lot smaller spectrum of possible parameter values for theta. 

# log(θ*exp(-θx)*λexp(-λθ))  = forallx(log(θ) -θx) + log(- λ)- λθ
# function that computes the bayesian model, is proportionate to p(θ|x)
bayesian <- function(theta,lambda, X) {
  return(dim(X)[1]*log(theta)-theta*sum(X) + log(lambda) -lambda*theta)
}

#Plot the curve showing the dependence of l(θ) on θ computed using the entire data, comparing with previous findings
curve(dim(machines)[1]*log(x)-x*sum(machines), 0, 25, xlab="θ", ylab="y", col="blue")
curve(dim(machines)[1]*log(x)-x*sum(machines) + log(10) -10*x, 0, 25, xlab="θ", ylab="log(p(x|θ))", col="red", add=TRUE)

# using optimize to get the maximum of the function and get optimal parameter value
optimize(bayesian, lambda =10, X= machines, c(0, 5), maximum= TRUE)$maximum

# set.seed(12345)

generating ra50 ndom values from the exponential distribution
random_vector <- rexp(50,1.126)


#making histograms to compare
hist(random_vector)
hist(machines$Length)