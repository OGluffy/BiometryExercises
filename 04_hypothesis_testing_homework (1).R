data <- read.csv(file.choose()) # classdata_onlyfall2025.csv
names(data)
data$Grad_topic <- as.factor(data$Grad_topic)
levels(data$Grad_topic)
# a. make a graph
# first subset to include only Biology and Pharmacy:
library(dplyr)
bio_pharm_only <- data %>%
  filter(Grad_topic=="Biology" | Grad_topic=="Pharmacy")
# factor Grad_topic after subsetting:
levels(bio_pharm_only$Grad_topic)

bio_pharm_only$Grad_topic <- factor(bio_pharm_only$Grad_topic)
# examine the levels:
levels(bio_pharm_only$Grad_topic)

library(sciplot)
bargraph.CI(bio_pharm_only$Grad_topic, bio_pharm_only$Dist_km, 
xlab="Topic of study", ylab="Distance of birthplace from Oxford (km) +/- SE",
main="Figure 1")

# b. State Hypotheses
# Null hypothesis: Biology and pharmacy students were born the same average distance from Oxford
# Alternative hypothesis: Biology and pharmacy students were NOT born the same average distance from Oxford

# c. Calculate observed test statistic
bio <- data %>%
  filter(Grad_topic=="Biology")
pharm <- data %>%
  filter(Grad_topic=="Pharmacy")
meandiff <- mean(bio$Dist_km)-mean(pharm$Dist_km)
meandiff

# d. Use loop to get p-value from randomization test
nulldata<-data		# create a new copy of the data frame that can be shuffled
nullmeandiffs <- rep(NA)	# create an empty vector for the null test statistics
yes <- 0	# initialize a variable for tallying extreme observations of the test stat

for (x in c(1:10000))		# 'loop' to generate 10,000 shuffled null samples
{nulldata$Dist_km <- sample(nulldata$Dist_km)
nullbio <- nulldata %>%
  filter(Grad_topic=="Biology")
nullpharm <- nulldata %>%
  filter(Grad_topic=="Pharmacy")
nullmeandiffs[x] <- mean(nullbio$Dist_km)-mean(nullpharm$Dist_km)
if (abs(nullmeandiffs[x])>=abs(meandiff)) {yes<-yes+1}
}

# generate histogram of null distribution:
hist(nullmeandiffs, main="Histogram of null distribution", xlab="Bio-Pharm distance difference (km)")
     
meandiff		# print the observed test statistic again
P<-yes/10000	# calculate the P-value
format(P,nsmall=5,scientific=F)	# print the P-value in nice format

# e. state conclusion
# We do not reject the null hypothesis. On average, pharmacy students 
# were NOT born a different distance from Oxford than biology students,
# in the 2025 class (P=0.095, Fig. 1).
