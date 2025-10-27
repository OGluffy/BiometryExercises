mydata <- read.csv(file.choose())
names(mydata)  # makes a list of your variable names, so you can see them easily
head(mydata)  # shows the top few rows of data
View(mydata)  # opens the whole data frame for viewing

hist(mydata$Height_in)
hist(mydata$Dist_km)

mean(mydata$Height_in, na.rm=T)

library(pastecs)
options(scipen = 999) # eliminates scientific notation
stat.desc(mydata$Dist_km)
