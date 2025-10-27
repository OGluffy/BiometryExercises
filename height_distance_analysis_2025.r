# ------------------------------------------------------------
# Biometry Analysis Script - Fall 2025
# Slide 20: Gender and Height Difference
# Slide 51: Grad Topic and Birth Distance Comparison
# ------------------------------------------------------------

# Load required libraries
library(dplyr)      # for data manipulation
library(sciplot)    # for bargraph.CI plots

# ------------------------------------------------------------
# PART 1: Gender and Height Difference (Slide 20)
# ------------------------------------------------------------

# Step 1: Prepare data
data_2025$gender <- as.factor(data_2025$gender)     # ensure gender is a factor
levels(data_2025$gender)                            # check factor levels

# Step 2: Subset by gender
maledata <- data_2025 %>% filter(gender == "Male")
femaledata <- data_2025 %>% filter(gender == "Female")

# Step 3: Calculate observed test statistic (mean difference)
meandiff <- mean(maledata$height_in, na.rm = TRUE) -
            mean(femaledata$height_in, na.rm = TRUE)
meandiff   # print observed difference

# Step 4: Visualize mean heights with error bars
bargraph.CI(data_2025$gender, data_2025$height_in,
            xlab = "Gender", ylab = "Height (inches)",
            main = "Mean Height by Gender (±1 SE)")

# ------------------------------------------------------------
# PART 2: Grad Topic and Birth Distance Comparison (Slide 51)
# ------------------------------------------------------------

# Step 1: Subset to Biology and Pharmacy students only
data_2025 <- data_2025 %>%
  filter(grad_topic == "Biology" | grad_topic == "Pharmacy")

data_2025$grad_topic <- factor(data_2025$grad_topic)  # drop unused levels

# Step 2: Split into two groups
bio <- data_2025 %>% filter(grad_topic == "Biology")
pharm <- data_2025 %>% filter(grad_topic == "Pharmacy")

# Step 3: Calculate observed test statistic (mean difference)
meandiff <- mean(pharm$distance_from_oxford_km, na.rm = TRUE) -
            mean(bio$distance_from_oxford_km, na.rm = TRUE)
meandiff   # print observed difference

# Step 4: Visualize mean distances with error bars
bargraph.CI(data_2025$grad_topic, data_2025$distance_from_oxford_km,
            xlab = "Grad Topic", ylab = "Distance from Oxford (km)",
            main = "Mean Birth Distance by Grad Topic (±1 SE)")

# Step 5: Simulate null distribution (2-tailed test)
nulldata <- data_2025
nullmeandiffs <- rep(NA, 10000)
yesno <- 0

for (x in 1:10000) {
  nulldata$distance_from_oxford_km <- sample(nulldata$distance_from_oxford_km)
  
  nullpharm <- nulldata %>% filter(grad_topic == "Pharmacy")
  nullbio <- nulldata %>% filter(grad_topic == "Biology")
  
  nullmeandiffs[x] <- mean(nullpharm$distance_from_oxford_km, na.rm = TRUE) -
                      mean(nullbio$distance_from_oxford_km, na.rm = TRUE)
  
  # 2-tailed comparison
  if (abs(nullmeandiffs[x]) >= abs(meandiff)) {
    yesno <- yesno + 1
  }
}

# Step 6: Plot null distribution and calculate P-value
hist(nullmeandiffs,
     main = "Null Distribution of Mean Differences",
     xlab = "Pharmacy - Biology Distance (km)")

meandiff   # print observed test statistic again

P <- yesno / 10000
format(P, nsmall = 5, scientific = FALSE)   # print P-value nicely

# ------------------------------------------------------------
# End of Script
# ------------------------------------------------------------
