# ------------------------------------------------------------
# Biometry Class Data - Correlation Analysis (Slide 42)
# ------------------------------------------------------------
# Goal: Analyze relationships among continuous variables: height, age, and distance
# ------------------------------------------------------------

# Load required libraries
library(dplyr)      # for data manipulation
library(wPerm)      # for randomization test
library(sciplot)    # optional for grouped plots

# ------------------------------------------------------------
# Step 1: Load the Biometry class dataset
# ------------------------------------------------------------

mydata <- read.csv(file.choose())  # Choose your Biometry class .csv file
names(mydata)                      # View variable names

# ------------------------------------------------------------
# Step 2: Visualize all 3 pairwise relationships with scatterplots
# ------------------------------------------------------------

pairs(~height + age + distance_from_oxford_km, data = mydata,
      main = "Scatterplot Matrix: Height, Age, Distance")

# ------------------------------------------------------------
# Step 3: Choose one pair to analyze: Height vs. Age
# ------------------------------------------------------------

# Step 4: Create a complete subset (remove rows with missing values)
complete_subset <- mydata %>%
  filter(!is.na(height) & !is.na(age))

# ------------------------------------------------------------
# Step 5: State hypotheses
# ------------------------------------------------------------
# H0: There is no correlation between height and age
# HA: There is a correlation between height and age (non-directional, 2-tailed)

# ------------------------------------------------------------
# Step 6: Check normality assumption with histograms
# ------------------------------------------------------------

hist(complete_subset$height, main = "Histogram of Height")
hist(complete_subset$age, main = "Histogram of Age")

# ------------------------------------------------------------
# Step 7: Choose appropriate correlation test
# ------------------------------------------------------------
# If both variables are approximately normal, use Pearson's r
# If not, use Kendall's tau or randomization test

# Option A: Pearson's correlation
cor.test(complete_subset$height, complete_subset$age, method = "pearson")

# Option B: Kendall's tau (non-parametric)
cor.test(complete_subset$height, complete_subset$age, method = "kendall")

# Option C: Randomization test
perm.relation(complete_subset$height, complete_subset$age)

# ------------------------------------------------------------
# Step 8: Report results (example format)
# ------------------------------------------------------------
# Height and age were [positively/negatively] correlated (r = ..., P = ..., N = ...).
# OR
# Height and age were significantly correlated (tau = ..., P < 0.001, N = ...).
# OR
# A randomization test showed a significant relationship between height and age (r = ..., P < 0.001, N = ...).

# ------------------------------------------------------------
# End of Script
# ------------------------------------------------------------



# ------------------------------------------------------------
# NEW Script different comparison age vs. distance ####
# ------------------------------------------------------------

# ------------------------------------------------------------
# Biometry Class Data - Correlation Analysis: Age vs. Distance
# ------------------------------------------------------------
# Goal: Analyze relationship between age and distance_from_oxford_km
# ------------------------------------------------------------

# Load required libraries
library(dplyr)      # for data manipulation
library(wPerm)      # for randomization test

# ------------------------------------------------------------
# Step 1: Load the Biometry class dataset
# ------------------------------------------------------------

mydata <- read.csv(file.choose())  # Choose your Biometry class .csv file
names(mydata)                      # View variable names

# ------------------------------------------------------------
# Step 2: Create a complete subset (remove rows with missing values)
# ------------------------------------------------------------

complete_subset <- mydata %>%
  filter(!is.na(age) & !is.na(distance_from_oxford_km))

# ------------------------------------------------------------
# Step 3: Visualize the relationship with a scatterplot
# ------------------------------------------------------------

plot(complete_subset$distance_from_oxford_km ~ complete_subset$age,
     xlab = "Age (years)", ylab = "Distance from Oxford (km)",
     main = "Scatterplot: Age vs. Distance")

# ------------------------------------------------------------
# Step 4: State hypotheses
# ------------------------------------------------------------
# H0: There is no correlation between age and distance from Oxford
# HA: There is a correlation between age and distance from Oxford (non-directional, 2-tailed)

# ------------------------------------------------------------
# Step 5: Check normality assumption with histograms
# ------------------------------------------------------------

hist(complete_subset$age, main = "Histogram of Age")
hist(complete_subset$distance_from_oxford_km, main = "Histogram of Distance")

# ------------------------------------------------------------
# Step 6: Choose appropriate correlation test
# ------------------------------------------------------------
# If both variables are approximately normal, use Pearson's r
# If not, use Kendall's tau or randomization test

# Option A: Pearson's correlation
cor.test(complete_subset$age, complete_subset$distance_from_oxford_km, method = "pearson")

# Option B: Kendall's tau (non-parametric)
cor.test(complete_subset$age, complete_subset$distance_from_oxford_km, method = "kendall")

# Option C: Randomization test
perm.relation(complete_subset$age, complete_subset$distance_from_oxford_km)

# ------------------------------------------------------------
# Step 7: Report results (example format)
# ------------------------------------------------------------
# Age was [positively/negatively] correlated with distance from Oxford (r = ..., P = ..., N = ...).
# OR
# Age and distance were significantly correlated (tau = ..., P < 0.001, N = ...).
# OR
# A randomization test showed a significant relationship between age and distance (r = ..., P < 0.001, N = ...).

# ------------------------------------------------------------
# End of Script
# ------------------------------------------------------------
