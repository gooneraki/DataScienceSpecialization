#Q1
#In a population of interest, a sample of 9 men yielded a sample average brain volume of 1,100cc and a standard deviation of 30cc. What is a 95% Student’s T confidence interval for the mean brain volume in this new population?
mbv <- 1100
p <- .95
s <- 30
n <- 9
ci <- mbv + c(-1,1) * qt(p+(1-p)/2, n-1) * s / sqrt(n)
ci

#Q2
#A diet pill is given to 9 subjects over six weeks. The average difference in weight (follow up - baseline) is -2 pounds. What would the standard deviation of the difference in weight have to be for the upper endpoint of the 95% T confidence interval to touch 0?
#upper_endpoint = (followup - baseline) + qt(p+(1-p)/2, n-1) * sd / sqrt(n)
#==> sd = (upper_endpoint - (followup - baseline)) * sqrt(n) / qt(p+(1-p)/2, n-1)
upper_endpoint <- 0
followup_less_baseline <- -2
p <- .95
n <- 9
sd <- (upper_endpoint - followup_less_baseline) * sqrt(n) / qt(p+(1-p)/2, n-1)
sd

#Q4
#In a study of emergency room waiting times, investigators consider a new and the standard triage systems. To test the systems, administrators selected 20 nights and randomly assigned the new triage system to be used on 10 nights and the standard system on the remaining 10 nights. They calculated the nightly median waiting time (MWT) to see a physician. The average MWT for the new system was 3 hours with a variance of 0.60 while the average MWT for the old system was 5 hours with a variance of 0.68. Consider the 95% confidence interval estimate for the differences of the mean MWT associated with the new system. Assume a constant variance. What is the interval? Subtract in this order (New System - Old System).
nnew <- 10
mnew <- 3
varnew <- .6

nold <- 10
mold <- 5
varold <- .68

p <- .95

pooled_variance <- ((nnew-1)*varnew + (nold-1)*varold) / (nnew + nold - 2)
ci <- (mnew - mold) + c(-1, 1) * qt(p + (1-p)/2, (nnew+nold-2)) * sqrt(pooled_variance) * sqrt(1/nnew + 1/nold)
ci

#Q6
#To further test the hospital triage system, administrators selected 200 nights and randomly assigned a new triage system to be used on 100 nights and a standard system on the remaining 100 nights. They calculated the nightly median waiting time (MWT) to see a physician. The average MWT for the new system was 4 hours with a standard deviation of 0.5 hours while the average MWT for the old system was 6 hours with a standard deviation of 2 hours. Consider the hypothesis of a decrease in the mean MWT associated with the new treatment. What does the 95% independent group confidence interval with unequal variances suggest vis a vis this hypothesis? (Because there’s so many observations per group, just use the Z quantile instead of the T.)
nnew <- 100
nold <- 100
mnew <- 4
mold <- 6
snew <- 0.5
sold <- 2
SE <- sqrt(snew^2/100 + sold^2/100)
ci <- (mold - mnew) + c(-1, 1) * qnorm(0.975) * SE
ci

#Q7
#Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo. Subjects’ body mass indices (BMIs) were measured at a baseline and again after having received the treatment or placebo for four weeks. The average difference from follow-up to the baseline (followup - baseline) was −3 kg/m2 for the treated group and 1 kg/m2 for the placebo group. The corresponding standard deviations of the differences was 1.5 kg/m2 for the treatment group and 1.8 kg/m2 for the placebo group. Does the change in BMI over the four week period appear to differ between the treated and placebo groups? Assuming normality of the underlying data and a common population variance, calculate the relevant 90% t confidence interval. Subtract in the order of (Treated - Placebo) with the smaller (more negative) number first.
mt <- -3
mp <- 1
st <- 1.5
sp <- 1.8
nt <- 9
np <- 9
p <- 0.9
pooled_variance <- ((nt-1)*st^2 + (np-1)*sp^2)/(nt + np - 2)
ci <- mt - mp + c(-1,1) * qt(p + (1-p)/2, df=(nt+np-2)) * sqrt(pooled_variance) * sqrt(1/nt + 1/np)
ci


