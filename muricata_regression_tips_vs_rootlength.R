# ------------------------------------------------------------
# Biometry Regression Analysis - Slide 54
# Dataset: muricata_modified2.csv
# Goal: Test if colonization (tips) is predicted by total root length (est.root.lgth)
# ------------------------------------------------------------

# Step 1: Load the dataset
data <- read.csv(file.choose())  # Choose muricata_modified2.csv
names(data)                      # View variable names

# Step 2: State hypotheses
# H0: There is no linear relationship between root length and colonization (slope = 0)
# HA: There is a linear relationship between root length and colonization (slope ≠ 0)

# Step 3: Fit the regression model
model <- lm(tips ~ est.root.lgth, data = data)

# Step 4: Visualize the relationship and check linearity
plot(data$tips ~ data$est.root.lgth,
     xlab = "Estimated Root Length",
     ylab = "Mycorrhizal Colonization (Tips)",
     main = "Scatterplot with Regression Line")
abline(model)  # Add best fit line

# Step 5: Check assumptions

# 5a. Normality of residuals
hist(model$residuals,
     main = "Histogram of Residuals",
     xlab = "Residuals")

# 5b. Homoscedasticity (equal variance)
plot(model$residuals ~ model$fitted.values,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values")

# 5c. Influence of outliers (Cook's Distance)
cooks <- cooks.distance(model)
max(cooks)  # Check maximum Cook's Distance

# If any Cook's Distance > 1, list influential observations
if (max(cooks) > 1) {
  influential <- cbind(data, cooks)
  influential[cooks > 1, ]
}

# Step 6: Examine regression output
summary(model)

# Step 7: Report results
# Extract coefficients
intercept <- coef(model)[1]
slope <- coef(model)[2]
r_squared <- summary(model)$r.squared
p_value <- summary(model)$coefficients[2, 4]

# Example conclusion (replace with actual values from output):
# "Total root length significantly predicted mycorrhizal colonization (tips),
# with the regression equation: tips = [intercept] + [slope] * est.root.lgth.
# R² = [r_squared], P = [p_value]."

**note** ask copilot, chatgpt or gemini to assit with the conclusion with actual results produced.
