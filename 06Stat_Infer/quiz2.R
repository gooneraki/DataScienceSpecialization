#Q1
#sigma^2 / n

#Q2
value2<- 70
mean2<- 80
sd2<- 10
answ2<-pnorm(value2, mean2, sd2)
answ2

#Q3
quantil3<- 0.95
mean3<- 1100
sd3<- 75
answ3<-qnorm(quantil3, mean3, sd3)
answ3

#Q4
mean4<- 1100
sd4<- 75
n4<- 100
var4<- sd4/sqrt(n4)
quantile4<- 1.645
answ4<- mean4+(var4*quantile4)
answ4

#Q5
prob4<- choose(5,4)*0.5^4*(1-0.5)^1
prob5<- choose(5,5)*0.5^5*(1-0.5)^0
answ5<- prob4+prob5
answ5

#Q6
mean6<- 15
sd6<- 10
value6a<- (14-mean6)/(sd6/sqrt(100))
p14<-pnorm(value6a)
value6b<- (16-mean6)/(sd6/sqrt(100))
p16<-pnorm(value6b)
answ6<-p16-p14
answ6
#or pnorm(16,mean=15,sd=10/sqrt(100))-pnorm(14,mean=15,sd=10/sqrt(100))

#Q8
answ8<-ppois(10,lambda=15)
answ8