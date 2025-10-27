# ------------------------------------------------------------
# Biometry Regression Analysis - Slide 54
# Dataset: muricata_modified2.csv
# Goal: Test if TotContam is predicted by tips
# ------------------------------------------------------------

# Step 1: Load the dataset
muricata <- read.csv(file.choose(), header = TRUE)
names(muricata)  # View variable names

# Step 2: Fit the initial regression model (parametric test)
model1 <- lm(TotContam ~ tips, data = muricata)

# Step 3: Visualize the relationship and check linearity
plot(muricata$TotContam ~ muricata$tips,
     xlab = "Rhizopogon Root Tips",
     ylab = "Contaminant Root Tips",
     main = "Scatterplot with Regression Line")
abline(model1)

# Step 4: Check assumptions

# 4a. Normality of residuals
hist(model1$residuals,
     main = "Histogram of Residuals",
     xlab = "Residuals")

# 4b. Homoscedasticity (equal variance)
plot(model1$residuals ~ model1$fitted.values,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values")

# 4c. Influence of outliers (Cook's Distance)
cooks <- cooks.distance(model1)
max(cooks)  # Check maximum Cook's Distance

# If any Cook's Distance > 1, list influential observations
if (max(cooks) > 1) {
  influential <- cbind(muricata, cooks)
  influential[cooks > 1, ]
}

# Step 5: Inspect regression output
summary(model1)

# ------------------------------------------------------------
# Step 6: Try transformations if assumptions are violated
# ------------------------------------------------------------

# Check distribution of TotContam
hist(muricata$TotContam, main = "Histogram of TotContam")
range(muricata$TotContam)  # Check for zeros

# Apply log(+1) transformation to TotContam
muricata$log_TotContam <- log(muricata$TotContam + 1)

# Re-run regression with transformed response
model2 <- lm(log_TotContam ~ tips, data = muricata)

# Check assumptions again
hist(model2$residuals, main = "Residuals of Transformed Model")
plot(model2$residuals ~ model2$fitted.values,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values (Transformed)")

# Cook's Distance for transformed model
cooks2 <- cooks.distance(model2)
max(cooks2)

# Summary of transformed model
summary(model2)

# ------------------------------------------------------------
# Step 7: Try randomization test (alternative to transformation)
# ------------------------------------------------------------

library(lmPerm)
perm_model <- lmp(TotContam ~ tips, data = muricata,
                  maxIter = 10000, perm = "Exact")
summary(perm_model)

# ------------------------------------------------------------
# Step 8: Compare results
# ------------------------------------------------------------
# Compare:
# - P-value from model1 (original)
# - P-value from model2 (transformed)
# - P-value from perm_model (randomization)

# Use these to decide whether the relationship is significant and which method is most appropriate
