# Example from lecture:
viagraData <- read.csv(file.choose()) # read in Viagra.csv
# notice that the dose variable is coded with numbers, but should be categorical, so
# we need to factor it:
viagraData$dose <- as.factor(viagraData$dose)

# run the ANOVA test:
out1 <-aov(libido~dose, data = viagraData)
summary(out1) # the ANOVA is significant (P<0.05), but we have not yet checked assumptions

# first check balance and replication:
info <- model.tables(out1, "means") # out1 is an aov() object
info$n # if design is balanced, you will only see one number, which is the n per group

# check normality:
hist(out1$residuals) # looks right-skewed, but...
# due to balanced design we can ignore this violation
# check homogeneity of variance:
plot(out1$residuals ~ out1$fitted.values) # homescedasticity looks fine

# if you are uncomfortable ignoring the violation of normality, you 
# could just use a randomization test:
library(lmPerm)
out2 <- aovp(libido ~ dose, data=viagraData, maxIter=10000, perm="Exact")
summary(out2) # very similar P-value as the ANOVA
# Conclusion: "Mean libido varied significantly among drug treatments (F2,12=5.12, P=0.025)."

# But now we need to describe the direction of the effect. Which treatment group(s) has/have
# different means than others? 
# Start by making a graph:
library(sciplot)
bargraph.CI(viagraData$dose, viagraData$libido, ylab='mean libido +/- SE')
# some differences are apparent, but are they significant?  see next class period


# ANOVA Homework Exercise

data<-read.csv(file.choose()) # read in the CMN data
names(data)

# Null hypothesis: Mean Final Mass does not differ among Seed Origins (or: Seed Origin has no influence on Final Mass)
# Alternative hypothesis: Mean Final Mass varies among Seed Origins (or: Seed origin influences Final Mass)

out1 <- aov(Final_mass ~ Seed_origin, data=data)

# check samples sizes in each group:
info <- model.tables(out1, "means") # out1 is an aov() object
info$n
# notice that sample sizes are NOT equal in each group, and are NOT all larger than 30

# check normality:
hist(out1$residuals) # looks right-skewed
# check homogeneity of variance:
plot(out1$residuals ~ out1$fitted.values) # looks fine

# normality assumption is not met, so we have two options:
# 1. use a randomization test
# 3. try transforming the response variable
# pick one and go with it--don't do both

# Here's how to do Option 3:
# Try transforming response variable with square root:
data$sqrt_Final_mass <- sqrt(data$Final_mass)
# re-run ANOVA using the transformed response variable
out2 <- aov(sqrt_Final_mass ~ Seed_origin, data=data)
# re-check normality of residuals:
hist(out2$residuals) # That's better
# Now re-check homogeneity of variance:
plot(out2$residuals ~ out2$fitted.values) # looks good
# now look at results in ANOVA table:
summary(out2)
# We cannot reject the null hypothesis.
# Conclusion: Mean final mass did not differ 
# among seed origins (F(2,66)=1.45, P=0.24).
# Or: Seed origin did not influence final mass of seedlings (F(2,66)=1.45, p=0.24).

# graph the data to show potential influence of Seed Origin on Final Mass
library(sciplot)
bargraph.CI(data$Seed_origin, data$Final_mass, ylim=range(0:9), ylab='mean Final Mass (g) +/- SE')


# Here's how to do Option 1:
library(lmPerm)
out3 <- aovp(Final_mass ~ Seed_origin, data=data, maxIter=10000, perm="Prob")
summary(out3)
# re-run it several times to make sure it is giving you a consistent decision about the null hypothesis.
# (it is)
# We cannot reject the null hypothesis.
# Conclusion: Mean final mass did not differ 
# among seed origins (df=2,66; P=0.26).
# Or: Seed origin did not influence final mass of seedlings (df=2,66; P=0.26).

# Note: if we had a violation of homoscedasticity (which we do not), we could use likelihood model fitting,
# as explained in lecture. Here would be the code for that:
library(nlme)
out4<-gls(Final_mass ~ Seed_origin, data = data, na.action=na.omit)
hist(resid(out4))  # to check normality of residuals
# in this example, the residuals are not normally distributed, so this analysis would not be a good choice
anova(out4)

