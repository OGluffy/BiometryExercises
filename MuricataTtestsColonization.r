# ------------------------------------------------------------
# Biometry Analysis Script - Slide 73
# Comparing Mycorrhizal Colonization (Tips)
# ------------------------------------------------------------
# Goal: Conduct two-sample t-tests to compare colonization across Fungus genotypes and Soil types
# ------------------------------------------------------------

# Load required libraries
library(dplyr)      # for data manipulation
library(sciplot)    # for bargraph.CI plots

# ------------------------------------------------------------
# Step 1: Load the muricata dataset
# ------------------------------------------------------------

data <- read.csv(file.choose())  # Choose muricata_modified2.csv

# View variable names
names(data)

# ------------------------------------------------------------
# Step 2: Factor categorical variables
# ------------------------------------------------------------

data$Fungus <- as.factor(data$Fungus)       # Fungus genotype: 132 or 133
data$Soil <- as.factor(data$Soil)           # Soil type: PtR or Pro

# ------------------------------------------------------------
# Step 3: Check normality assumption for Fungus comparison
# ------------------------------------------------------------

fungus_132 <- subset(data, Fungus == "132")
fungus_133 <- subset(data, Fungus == "133")

hist(fungus_132$tips, main = "Colonization Tips - Fungus 132")
hist(fungus_133$tips, main = "Colonization Tips - Fungus 133")

# ------------------------------------------------------------
# Step 4: Graph colonization by Fungus genotype
# ------------------------------------------------------------

bargraph.CI(data$Fungus, data$tips,
            xlab = "Fungus Genotype",
            ylab = "Mycorrhizal Colonization (Tips)",
            main = "Colonization by Fungus Genotype (±1 SE)")

# ------------------------------------------------------------
# Step 5: Conduct Welch's t-test for Fungus genotype
# ------------------------------------------------------------

t.test(tips ~ Fungus, data = data)  # Welch's t-test (default)

# ------------------------------------------------------------
# Step 6: Check normality assumption for Soil comparison
# ------------------------------------------------------------

soil_PtR <- subset(data, Soil == "PtR")
soil_Pro <- subset(data, Soil == "Pro")

hist(soil_PtR$tips, main = "Colonization Tips - Soil PtR")
hist(soil_Pro$tips, main = "Colonization Tips - Soil Pro")

# ------------------------------------------------------------
# Step 7: Graph colonization by Soil type
# ------------------------------------------------------------

bargraph.CI(data$Soil, data$tips,
            xlab = "Soil Type",
            ylab = "Mycorrhizal Colonization (Tips)",
            main = "Colonization by Soil Type (±1 SE)")

# ------------------------------------------------------------
# Step 8: Conduct Welch's t-test for Soil type
# ------------------------------------------------------------

t.test(tips ~ Soil, data = data)  # Welch's t-test (default)

# ------------------------------------------------------------
# End of Script
# ------------------------------------------------------------
