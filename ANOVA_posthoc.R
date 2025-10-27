cmn<-read.csv(file.choose())
names(cmn)
cmn$Seed_origin <- as.factor(cmn$Seed_origin) 
# always factor your categorical variables

out1 <- aov(Initial_Ht~Seed_origin, data=cmn)
# check assumptions:
# first calculate balance and replication:
info <- model.tables(out1, "means") # out1 is an aov() object
info$n
# sample size is large in each group (61,33,97), so probably don't need to worry about normality
hist(out1$residuals) # normality looks good
plot(out1$residuals ~ out1$fitted.value) # homoscedasticity
# looks good also, but note that variance seems slightly larger
# in the 3rd group, which may make the test a bit more 
# conservative (see slide 35 from Intro to ANOVA)

# so, proceed with ANOVA
summary(out1) # shows ANOVA table
# ANOVA is highly significant, so reject null hypothesis that
# mean Initial Height is equal among the 3 Seed Origins.
# Conclusion, Part 1: "Mean Initial Height differs among the three Seed Origins (F(2,188)=41.4, P<0.001).

# need to follow up this significant 1-way ANOVA with
# post-hoc tests or planned contrasts to know which means differ
# from which. Normally, you should only choose ONE of those
# two options, depending on the situation. Here, we'll 
# demonstrate both.

# first let's explore the first scenario, in which we don't
# have any a priori hypotheses, so we will use Tukey HSD post-hoc tests:
TukeyHSD(out1)  # gives Tukey HSD post-hoc tests
# shows that Swanton is significantly larger than the other 
# two, but Guadalupe and Cambria are not significantly different
# Here's an alternative method of doing Tukey HSD, which also
# gives the letters
library(multcomp)
postHocs <- glht(out1, linfct = mcp(Seed_origin = "Tukey"))
summary(postHocs)
cld(postHocs)

library(sciplot)
bargraph.CI(cmn$Seed_origin, cmn$Initial_Ht, ylim=c(0,60),ylab="Initial Ht. (cm) +/-SE", xlab="Plant Population of Origin")
text(locator(3), c("A", "A", "B"))
Conclusion, Part 2 (post-hoc test version): "Specifically, Swanton had a larger initial height than the other two populations (Fig. 1)."

# Now let's explore the second scenario, in which you have 
# a priori hypotheses, by using planned (a priori) contrasts:
# set a priori contrasts:
levels(cmn$Seed_origin)  # to show spelling & order of levels
contrasts(cmn$Seed_origin)<-cbind("Swanton - others"=c(-1, -1, 2), "Cambria - Guadalupe"=c(1, -1, 0))
# check that the contrasts are properly set by looking at the 
# attributes of the variable:
attributes(cmn$Seed_origin)
# now re-run analysis and print results of contrasts:
out3 <- aov(Initial_Ht~Seed_origin, data=cmn)
summary.lm(out3) # shows results of contrasts
# results show that both contrasts are significant. So, 
# Conclusion, Part 2 (contrast version): 
# Mean Initial Ht at Guadalupe is significantly larger than for
# Cambria (p=0.033), and mean Initial Height for Swanton is larger
# than the mean of the other two Seed Origins (p<0.0001) (Fig. 1).
# How would you indicate those results on the bar graph?
# Why is the comparison between Guadalupe and Cambria significant with a priori contrasts, but not with Tukey HSD post-hoc tests?


