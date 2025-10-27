# For Question 1 in the t-test exercise (Did RGR differ significantly from a null hypothesis of 0.01?):
# a) Calculate observed effect size
# one-sample t-test, with RGR compared to 0.01
x1 <- mean(muricata$RGR, na.rm=T)
x2 <- 0.01
s <- sd(muricata$RGR, na.rm=T)

library(pastecs)
stat.desc(muricata$RGR)

d_obs <- (x1-x2)/s
d_obs
# Answer = 1.76

# b) Calculate what was achieved power
library(pwr)
pwr.t.test(n = 128, d = d_obs , sig.level = 0.05 , power = NULL, type = "one.sample")
# Answer: Power is 1.0

# c) Estimate what sample size is needed in a future experiment
pwr.t.test(n = NULL, d = d_obs , sig.level = 0.05 , power = 0.7, type = "one.sample")
# Answer: N=5

# d) In a future experiment with only 30 observations, what is the smallest effect size you can expect to detect?
pwr.t.test(n = 30, d = NULL , sig.level = 0.05 , power = 0.7, type = "one.sample")  
# Answer: 0.47

# For Question 3 in the t-test exercise (Did mycorrhizal colonization (tips) differ between the 2 Soil types?):
# a) Calculate observed effect size
names(muricata)
dataPro <- subset(muricata, Soil=='Pro')
dataPtR <- subset(muricata, Soil=='PtR')

library(pastecs)
stat.desc(dataPro$tips) # gives descriptive stats for Group 1
stat.desc(dataPtR$tips) # gives descriptive stats for Group 2
x1<-238.5 # mean in group 1 (Pro soil)
x2<-131.459 # mean in group 2 (PtR soil)
s1<-102.82 # SD in group 1
s2<-100.8325 # SD in group 2
df1<-63 # df in group 1
df2<-60 # df in group 2
s<-sqrt((df1*s1^2 + df2*s2^2)/(df1+df2))   # pooled SD
s    # print the pooled SD
d_obs<-(x1-x2)/s  # calculate observed effect size
d_obs # print observed effect size
# Answer: 1.050912

# b) Calculate what was achieved power
library(pwr)
pwr.t2n.test(n1 = 64, n2=61, d = d_obs , sig.level = 0.05 , power = NULL)
# Answer: Power = 0.9999449

# c) Estimate what sample size is needed in a future experiment
pwr.t.test(n = NULL, d = d_obs , sig.level = 0.05 , power = 0.7, type = "two.sample")
# Answer: N=13 (in each group)

# d) In a future experiment with only 20 observations in each group, what is the smallest effect size you can expect to detect?
pwr.t.test(n = 20, d = NULL , sig.level = 0.05 , power = 0.7, type = "two.sample")
# Answer: 0.81
  

# Question 1
library(pwr)
# for regression:
pwr.f2.test(u = 1, v = NULL, f2 = 0.15, sig.level = 0.05, power = 0.7) 
# Answer: because v is 41.1, and u=1, our total df is 42.1, so our N is 43.1, but
# you should round up to 44, so N=44 is the answer

# for correlation:
pwr.r.test(n=NULL,power=0.7,sig.level=0.05,r=0.3)
# Answer: N=67

# Question 2
# for regression:
pwr.f2.test(u = 1, v = 13, f2 = NULL, sig.level = 0.05, power = 0.7) 
# Answer: 0.48

# for correlation:
pwr.r.test(n=59,power=0.7,sig.level=0.05,r=NULL)
# Answer: 0.32

#Question 3
muricata<- read.csv(file.choose(), header=T)
names(muricata)

#observed effect size:
cor.test(muricata$tips, muricata$total.weight, method='pearson')
robs <- 0.2738833

library(pwr)
#calculate achieved power:
pwr.r.test(r=robs, power=NULL,n=123,sig.level=0.05)
# Answer: Power=0.87

# calculate sample size required in a future experiment:
pwr.r.test(r=robs, power=0.7,n=NULL,sig.level=0.05)
# Answer: n=81

#Question 4
out1 <-lm(RGR ~ tips, data=muricata)
summary(out1)

#observed effect size:
f2 <- 0.00302/(1-0.00302)
f2
# Answer: f2 = 0.0030

#achieved power:
pwr.f2.test(u=1,v=123,power=NULL,sig.level=0.05,f2=0.003029148)
# answer: 0.094

# sample size in future experiment:
#achieved power:
pwr.f2.test(u=1,v=NULL,power=0.7,sig.level=0.05,f2=0.003029148)
# answer: 2040
