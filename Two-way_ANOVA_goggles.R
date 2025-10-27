# this is the beer goggles example from the 2-way ANOVA lecture

gogglesData <- read.csv(file.choose()) # read in goggles.csv
names(gogglesData)

#check order of levels for alcohol and change if necessary
gogglesData$alcohol <- as.factor(gogglesData$alcohol) # only do this for the categorical variables
levels(gogglesData$alcohol)
gogglesData$alcohol <- relevel(gogglesData$alcohol, ref="None")
levels(gogglesData$alcohol)
# this step of releveling is often not necessary!  only do it if needed,
# to make a control treatment the reference level

# fit the 2-way ANOVA model
gogglesModel<-aov(attractiveness ~ alcohol*gender, data = gogglesData)
# check balance and replication:
info <- model.tables(gogglesModel) # to calculate balance/replication
info$n  # shows balance/replication
# use same assumption guidelines as for one-way ANOVA
# check assumptions:
hist(gogglesModel$residuals) # looks fine
plot(gogglesModel$residuals~jitter(gogglesModel$fitted.values,600)) # maybe a problem, but our balanced design 
# means we can ignore it

# Now look at the Type II SS ANOVA table:
library(car)
Anova(gogglesModel, type="II")
# notice that the interaction is significant, so that is our main result
# the main effect of alcohol is significant too, but we will only
# discuss it if it is dramatic, and very apparent on the graph

# Graph means +/- SE for the alcohol x gender interaction:
library(sciplot)
bargraph.CI(gogglesData$alcohol, 
            gogglesData$attractiveness,
            gogglesData$gender, legend=T,
            x.leg=6.5,y.leg=72)
# change values of x.leg and y.leg to move legend

# now we need to annotate this graph to show which means are different than which; this will
# facilitate writing our Conclusions statement.

# If you have no a priori hypotheses about particular treatments, then
# just conduct Tukey HSD tests like this (gives Tukey tests for main effects and interactions):
TukeyHSD(gogglesModel)
# what letters would you use to annotate the graph? how would you put them on the graph?
# you could carefully go through the "p adj" column in the bottom table of the Tukey HSD results,
# and figure out which means are different from the others.

# or you can use this code to get the letters:
postHocs<-TukeyHSD(gogglesModel)
library(multcompView)
lets<-multcompLetters(postHocs$'alcohol:gender'[,4] )
lets

# Then add letters to the graph, ensuring that means only share a letter 
# if they are significantly different. You could do this in Powerpoint, or
#  or add them to your graph in R using this code:
text(locator(6), c("A", "A", "A", "A", "A", "B"))

# If instead you DID have a priori hypotheses, test them with a priori contrasts (as we did with 1-way ANOVA)
# My recommended approach to this, for significant interactions, is to use 'simple effects' contrasts:
library(emmeans)
emmeans(gogglesModel, pairwise ~ gender | alcohol)
# this only works if one of your categorial predictors has only 2 levels (like gender here)
# how would you indicate these results on a graph?  paste the graph into Powerpoint, and add
# indication of significance for each pairwise test between the 2 genders

# Conclusion: Attractiveness is affected by an interaction between alcohol consumption and gender
# (F2, 42 = 11.91, p < 0.001). Specifically, males and females did not differ in partner 
# attractiveness when drinking no alcohol or two pints, but partner attractiveness was lower 
# for males than females after drinking 4 pints (Fig. 1).

# What about the main effect of Alcohol, which is significant? Since it is quite
# apparent on the interaction graph, you should also write a conclusion statement 
# about it:

# Conclusion: Attractiveness is lower, overall, after 4 Pints of alcohol compared to
# the other two treatments (F2, 42 = 20.07, P< 0.001; Fig. 1)

# Optionally, you can make a new graph specifically to illustrate this significant main 
# effect of alcohol. Because we have a balanced design, and do not have any 
# observational predictor variables, we don't need to bother using marginal means.
# So, we can just graph using bargraph.CI:

library(sciplot)
bargraph.CI(gogglesData$alcohol, gogglesData$attractiveness)
# This graph then should be annotated with results from Tukey HSD post-hoc tests,
# or a priori contrasts







