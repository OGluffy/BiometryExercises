mydata <- read.csv(file.choose()) # read in the muricata_modified.csv file
names(mydata)

# Null hypothesis: tips is NOT predicted by a linear relationship 
# with root length (i.e., the slope is 0).

# Alternative hypothesis: tips is predicted by a linear relationship
# with root length (i.e., the slope is NOT 0).

# fit the regression model:
out.1 <- lm(tips ~ est.root.lgth, data = mydata)

par(mfrow=c(2,2)) # optional step to put your graphs into a 2x2 panel format

# check assumptions of linear regression:

# to check the linearity assumption, first make a scatterplot with the best fit line:
plot(mydata$tips ~ mydata$est.root.lgth)
abline(out.1) # linearity looks fine

# Next, we should check normality of residuals
# with a histogram of the residuals
hist(out.1$residuals) # this looks slightly right-skewed, but
# not enough to worry about

# Now check homogeneity of variances by plotting residuals vs. predicted (fitted) Y's
plot(out.1$residuals~ out.1$fitted.values) # this looks fine
plot(out.1$residuals~ mydata$est.root.lgth) # this looks fine

# now check for influential outliers
cooks <- cooks.distance(out.1)
#cooks  
max(cooks) # no problem  here, all Cook's Distance values are less than 1
# since max < 1, this assumption is OK

# since all assumptions are met, we can look at the results:
summary(out.1)
# Conclusion: Number of mycorrhizal root tips (tips) is negatively associated with
# estimated root length (slope=-0.305, intercept=252.9, P=0.005), although estimated
# root length only explains a low proportion of the variation in number of mycorrhizal
# tips (R-squared = 0.055).

