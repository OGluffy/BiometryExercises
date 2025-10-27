muricata<- read.csv(file.choose())
names(muricata)

# Null hypothesis: TotContam is NOT predicted by a linear relationship
# with tips (i.e., the slope is 0)
# Alternative hypothesis: TotContam is predicted by a linear relationship
# with tips (i.e., the slope is NOT 0)

muricata1 <- subset(muricata, TotContam!='NA' & tips!='NA')

out.1 <-lm(TotContam ~ tips, data=muricata1)

# check assumptions of linear regression:

# first make a scatterplot with the best fit line:
plot(muricata1$TotContam ~ muricata1$tips)
abline(out.1) # the linearity assumption seems fine

# Next, we should check normality and equality of
# variances for Y across all values of X
# with a histogram of the residuals
hist(out.1$residuals) # this assumption is VIOLATED--the residuals are highly right-skewed

# Now check equal variances by plotting residuals vs. predicted (fitted) Y's
plot(out.1$residuals~ out.1$fitted.values) # this assumption seems fine

cooks <- cooks.distance(out.1)
#cooks  # type cooks to see a list of them
max(cooks) # since no Cook's distances are greater than 1, this assumption is met

# residuals are right-skewed, so we have a violation. Let's try transformation.
# inspect histogram of predictor variable to see if that
# might be the source of the problem:

hist(muricata1$tips)  # shows somewhat skewed distribution
# this makes me think of using the square-root transformation

# inspect histogram of response variable to see if that
# might be the source of the problem:
hist(muricata1$TotContam) # shows very skewed distribution
# brings to mind the natural log transformation

# check range of both, to see if there are zeros or negative values:
range(muricata1$tips)
range(muricata1$TotContam)

# apply square-root transformation to the tips variable
muricata1$sqrt_tips <- sqrt(muricata1$tips)

# since there are zeros in both variables, use log(x+1) when applying
# the log transformation
muricata1$log_Contam <- log(muricata1$TotContam + 1)

# let's now try re-running the regression with those transformed variables:
out.2 <- lm(log_Contam~sqrt_tips, data=muricata1)

hist(out.2$residuals) # we still have an ugly histogram
# I wonder which variable is driving this problem?
hist(muricata1$sqrt_tips) # does tips look better now? probably good enough?
hist(muricata1$log_Contam) # does Contam look better now?  ew. not so good.

# Because log_Contam seems to be our problem, ...
# try a different transformation on TotContam, the square root:
muricata1$sqrt_Contam <- sqrt(muricata1$TotContam)

out.3 <- lm(sqrt_Contam~sqrt_tips, data=muricata1)
hist(out.3$residuals) # we have a less ugly histogram? Maybe
# normality is OK?

hist(muricata1$sqrt_Contam) # how does Contam look now?
# meh, but the residuals look better

# Conclusion: The transformation is working well for 
# tips, but not great for Contam. But remember: what we care about
# is the normality of the residuals from the model.
# So, let's re-run the model with the transformed variables, 
# and check the histogram of residuals.

# first make a scatterplot with the best fit line:
plot(muricata1$sqrt_Contam ~ muricata1$sqrt_tips)
abline(out.3) # looks fine
# Next, we should check normality and equality of
# variances for Y across all values of X
# with a histogram of the residuals
hist(out.3$residuals)  # again, looks a bit weird, but fine

# Now check equal variances by plotting residuals vs. predicted (fitted) Y's
plot(out.3$residuals~ out.3$fitted.values) # looks fine

cooks3 <- cooks.distance(out.3)
#cooks  # type cooks to see a list of them
max(cooks3) # fine

# The assumptions are met now, so we can use the regression on
# the transformed data. Look at the results:
summary(out.3)

# Conclusion: There is NOT a linear relationship between mycorrhizal tips and contamination (P=0.67)

# Just to demonstrate the code, here is a randomization test on the original data, which you
# could have used instead of trying transformations:
library(lmPerm)
out.4 <- lmp(TotContam~tips, data=muricata1, maxIter=10000, perm="Exact")
summary(out.4)
