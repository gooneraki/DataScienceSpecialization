#In this project we will investigate the exponential distribution in R and compare it with the Central Limit Theorem. We will perform 1000 simulations, each of consisting of 40 exponentials.

#We will investigate the following the following:
#1. Compare the sample mean to the theoretical mean of the distribution
#2. Compare the sample mean variance with the theoretical mean variance of the distribution
#3. Show that the distribution is approximately normal

#First we create the variables for our project
lambda<- 0.2
n<-40
no_sims<-1000
set.seed(3231)


#Investigation 1
expected_mean <- 1/lambda
expected_mean

sample_means <-replicate(no_sims ,mean(rexp(n, lambda)))
mean_of_means <- mean(sample_means)
mean_of_means

#The sample mean is 4.988 which is very close to the theoretical mean of 5

#Investigation 2
expected_sd<-(1/lambda)/sqrt(n)
expected_sd

sample_means_sd<-sd(sample_means)
sample_means_sd

#Sample mean variance is 0.775 which is very close to the theoretical mean variance of 0.791


#Investigation 3
#Lets plot our results
hist(sample_means,main="Distribution of Sample Means",xlab="sample mean",ylab="Density",breaks=25,freq=FALSE)
abline(v= expected_mean,col="red",lwd=2)
abline(v= mean_of_means,col="blue",lwd=2)
curve(dnorm(x, mean= mean_of_means, sd= sample_means_sd), col="blue", lwd=2, add=TRUE)
curve(dnorm(x, mean= expected_mean, sd= expected_sd), col="red", lwd=2, add=TRUE)
legend('topright', c("Simulated Values", "Theoretical Values"),col=c("blue","red"),lwd=2)

#Looking at the distribution of Sample means we can see that it rather matches the normal distribution plotted using the theoretical mean and sample variance values(red curved line). To verify this even more we can plot the QQ-Plot. We can see that the theoretical quantiles closely match the sample quantiless

qqnorm(sample_means,main ="Normal Q-Q Plot")
qqline(sample_means, col = "red")
 

 


