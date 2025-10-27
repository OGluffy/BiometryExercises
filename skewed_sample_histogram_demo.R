# ------------------------------------------------------------
# Histogram-Based Normality Check - Slides 25–28
# Goal: Show inconsistency of small sample histograms from a skewed population
# ------------------------------------------------------------

# Step 1: Create a skewed population using log-normal distribution
set.seed(2025)  # for reproducibility
skewdata <- rlnorm(10000)  # right-skewed population

# Step 2: Visualize the full population
par(mfrow = c(2, 2))  # set up 2x2 plot panel
hist(skewdata,
     main = "Skewed Population (n = 10,000)",
     xlab = "Value",
     col = "lightblue")

# Step 3: Draw a small sample (n = 8) and inspect histogram
samp_data <- sample(skewdata, 8)
hist(samp_data,
     main = "Sample Histogram 1 (n = 8)",
     xlab = "Value",
     col = "lightgreen")

# Step 4: Repeat sampling and histogram inspection 4 more times
for (i in 2:5) {
  samp <- sample(skewdata, 8)
  hist(samp,
       main = paste("Sample Histogram", i, "(n = 8)"),
       xlab = "Value",
       col = "lightcoral")
}

# ------------------------------------------------------------
# Interpretation:
# - Compare histogram shapes across samples
# - Note variability in appearance despite same population
# - Reinforces slide guidance: use histograms and common sense
# ------------------------------------------------------------
