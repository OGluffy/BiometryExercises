data <- read.csv(file.choose()) # read in muricata_modified2.csv data
names(data)
data$Plant <- as.factor(data$Plant)
data$Soil <- as.factor(data$Soil)
levels(data$Plant)
levels(data$Soil)

# fit the 2-way ANOVA model
out1 <- aov(tips ~ Soil + Plant + Soil:Plant, data = data)
# check balance and replication:
info <- model.tables(out1)
info$n
# notice that the design is nearly balanced, and we have high 
# replication (32) in each group

# check assumptions:
hist(out1$residuals) # normality looks fine
plot(out1$residuals~jitter(out1$fitted.values,10))
# homoscedasticity looks pretty good. variance might be slightly
# higher for the larger predicted values, but not too bad, and
# our design is nearly balanced, so we can ignore this small
# violation
# So, assumptions of normality and equal variances are fine

# Now look at the Type II SS ANOVA table:
library(car)
Anova(out1, type="II")
# notice that the interaction is significant, so that is our main result

# Graph means +/- SE for the Soil x Plant interaction:
library(sciplot)
bargraph.CI(data$Soil, 
            data$tips,
            data$Plant, legend=T,
            x.leg=3.5,y.leg=250,
            ylab="mean tips +/- SE",
            xlab="Soil",
            ylim=c(0,300))
# change values of x.leg and y.leg to move legend

# If you have no a priori hypotheses about particular treatments, then
# just conduct Tukey HSD tests like this (gives Tukey tests for main effects and interactions):
TukeyHSD(out1)
# what letters would you put on the graph?   A,A,B,A

#optionally use this code to get the Tukey letters:
postHocs<-TukeyHSD(out1)
#extract letters for graph
library(multcompView)
lets<-multcompLetters(postHocs$'Soil:Plant'[,4] )
lets  # could stop here and use these letters in Powerpoint, or add them to
# your graph in R using this code:
text(locator(4), c("A", "A", "B", "A"))

# Conclusion: Mycorrhizal colonization (tips) was affected by an interaction
# between Soil and Plant (F(1,121)=8.26, P=0.005), whereby
# in Pro soil there was no difference between the 2 plant 
# genotypes, but in PtR soil, mycorrhizal colonization was higher
# for the M19 plant genotype than for M18 (Fig. 1).

# Since the main effect of Soil also seems obvious in the graph, you
# should include this additional conclusion:
# Mycorrhizal colonization (tips) was also significantly lower in PtR soil than
# in Pro soil, overall (F(1,121)=38.5, P<0.001).

# If you DID have a priori hypotheses (for example: we hypothesize
# that the 2 plant genotypes would react differently to mycorrhizal 
# fungi in the two soils), test them with a priori contrasts (as we did with 1-way ANOVA)
# One good approach to this, for significant interactions, is to use "simple effects" contrasts:
library(emmeans)
emmeans(out1, pairwise ~ Plant | Soil)
# how would you annotate the graph to illustrate these results?

# Conclusion: Mycorrhizal colonization (tips) was affected by an interaction
# between Soil and Plant (F(1,121)=8.26, P=0.005), whereby
# in Pro soil there was no difference between the 2 plant 
# genotypes, but in PtR soil, mycorrhizal colonization was higher
# for the M19 plant genotype than for M18 (Fig. 1).














