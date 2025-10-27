# ANCOVA EXERCISE WITH MURICATA DATA

mur.data<-read.csv(file.choose()) # muricata_modified2.csv
names(mur.data)
mur.data$Soil <- as.factor(mur.data$Soil)
levels(mur.data$Soil)

# Null hypothesis 1: Mean est.root.lgth is the same for both levels of Soil

# Null hypothesis 2: Initial.gr.ht has no effect on est.root.lgth

# Null hypothesis 3: The effect of Soil on the response (est.root.lgth) is independent
# of Initial.gr.ht (and vice versa).  Or: Soil and Initial.gr.ht have independent effects on
# est.root.lgth.

# Step 1: fit full model with interaction
out1<-aov(est.root.lgth ~ Initial.gr.ht + Soil+ Initial.gr.ht:Soil, data=mur.data)

# check balance & replication:
info <- model.tables(out1) # to calculate balance/replication
info$n 

# now check assumptions:
hist(out1$residuals) # normality looks fine, although a bit left-skewed
plot(out1$residuals~jitter(out1$fitted.values, 10)) # violation of the homoscedasticity assumption
# check for influential outliers:
cooks <- cooks.distance(out1)
max(cooks)  # max value is 0.16, so this assumption is OK

# because homoscedasticity assumption is not met, we should try transforming one or 
# both of the numerical variables. Let's start with the response variable:
hist(mur.data$est.root.lgth)  # somewhat right-skewed, so try square-root:
range(mur.data$est.root.lgth) # check that all values are positive
mur.data$sqrt_est.root.lgth <- sqrt(mur.data$est.root.lgth)

hist(mur.data$Initial.gr.ht)
mur.data$sqrt_Initial.gr.ht <- sqrt(mur.data$Initial.gr.ht)

# now re-run model with transformed response variable:
# Step 1: fit full model with interaction
out2<-aov(sqrt_est.root.lgth ~ sqrt_Initial.gr.ht + Soil+ sqrt_Initial.gr.ht:Soil, data=mur.data)

# check balance & replication:
info <- model.tables(out2) # to calculate balance/replication
info$n 

# now check assumptions:
hist(out2$residuals) # normality looks fine, although a bit left-skewed
plot(out2$residuals~out2$fitted.values) # mild violation of the homoscedasticity assumption, so
# we will proceed with this model
# check for influential outliers:
cooks <- cooks.distance(out2)
max(cooks)  # max value is 0.16, so this assumption is OK

# Now, proceed to examining the output from the full ANCOVA model:
library(car)
Anova(out2, type="II")

# Note: The interaction is NOT significant, so the correct procedure is
# to move on to Step 2 (below); however, IF the interaction WAS significant,
# we would stop here, and make a graph, and discuss the interaction.
# Here is the code for that graph:
library(interactions)
interact_plot(out2, pred = "sqrt_Initial.gr.ht", modx = "Soil", plot.points = TRUE)


# BUT, interaction is not significant, so move on to...
# Step 2: reduced model without interaction
out3<-aov(sqrt_est.root.lgth~sqrt_Initial.gr.ht + Soil, data=mur.data)
hist(out3$residuals) # normality looks fine, although a bit left-skewed
plot(out3$residuals~out3$fitted.values) # only mild violation of the homoscedasticity
# check for influential outliers:
cooks <- cooks.distance(out2)
max(cooks) 


Anova(out3, type="II")
# Notice that both main effects are significant, so this is your
# final model. Need to describe these effects with graphs and Conclusion
# statements

# First focus on graphing and describing the results for Soil
# Because our model contains an observational predictor (Initial.gr.ht),
# we need to use *marginal* means for Soil. First obtain the marginal means (and SE):
library(emmeans)
emmeans(out3, "Soil")
# The resulting table of means and SE can be used in Excel or other
# software for making a bar graph, or optionally you can use them in
# R with the ggplot function (see below).

means1<-data.frame(emmeans(out3, "Soil"))
levels <- levels(means1$Soil)
adjMeans <- as.numeric(means1$emmean)
adjSE <- as.numeric(means1$SE)

library(ggplot2)
# define top and bottom of error bars:
limits <- aes(ymax = adjMeans + adjSE, ymin=adjMeans - adjSE)
# create mean / error bar plot
barDose<-ggplot(means1, aes(levels, adjMeans))
barDose+geom_bar(stat='identity', fill='White', color='Black')+
  geom_errorbar(limits, width=0.2)+labs(x='Soil', y='mean final root length +/- SE')
# Note: If Soil had more than 2 levels, you would need to do
# Tukey HSD post-hoc tests or a priori contrasts here, and 
# mark their results on the graph.

# Conclusion: Final root length is significantly larger for PtR soil than Pro soil
# (F(1,125)=207.8, P<0.001, Fig. 1).

# Now focus on graphing and making conclusions on the covariate, Initial.gr.ht.
# First inspect slope:
summary.lm(out3) # you can see that the slope is positive (1.95).
#Now make partial regression plot to illustrate this positive effect of the covariate:
library(car)
avPlots(out3, ~ sqrt_Initial.gr.ht)

# Conclusion: Square root of initial Height is positively associated with
# square root of final root length, when accounting for Soil (F(1,125)=21.3, 
# P<0.001, partial regression coefficient=1.95, Fig. 2).





