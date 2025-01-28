#Exercise sheet 4

#Question a. Create regression of wkswork against age and chlt5
attach(cps)
d1 <- lm(wkswork~age + chlt5)
d1
# the results reject the H0, statistical significant.
#The interpretation of chlt5 coefficient: average predicted increase in number of weeks worked last year for 
# a person without small children compared to a person with a small child, at any given age.

#Question b. Add the categorical variable region to the regression. South as baseline

##Question b. Substep 1. Reworking variable "region" to make South baseline

region <- relevel(region, ref="South")

##Question b. Substep 2. Creating new regression.
d2 <- lm(wkswork~age+chlt5+region)
d2

#Question c. Testing the hypothesis that coefficients on “Northeast” 
# and “West” variables are jointly zero at 5% significance level
install.packages("carData")
library(car)
lin_hyp2 <- linearHypothesis(d2, "regionNortheast + regionWest = 0")
lin_hyp2

#Conclusion: Pr(>F) < 0.05, therefore reject H0.

#Question d. Making West baseline in the variable "region"
#and re-estimating the regression
region <- relevel(region, ref="West")
d3 <- lm(wkswork~age+chlt5+region)
d3
#In this equation, the coefficient of "Northeast" is -0.4555 compared to the  -1.26499 estimated in question (b)
#The difference implies that at any given age and at same child group, 
#on expected average, the US female workers lived in the northeast would work 0.4555 weeks fewer than those lived in west region, and would work 1.26499 weeks fewer than those in south region


#Question e. Testing hypothesis that the coefficient on “Midwest” 
# is zero for d2 and d3

lin_hyp_2.1 <- linearHypothesis(d2, "regionMidwest = 0")
lin_hyp_2.1

lin_hyp_3 <- linearHypothesis(d3, "regionMidwest = 0")
lin_hyp_3

#Question f. Estimating the model
# wkswork = α + β1age + β2chlt5 + β3age × chlt5 + ε

d4 <- lm(wkswork ~age*chlt5)
d4

#Question g. Using the regression d4, test the null hypothesis 
# that coefficient on age*chlt5 = 0

lin_hyp_4 <- linearHypothesis(d4, "age:chlt5No small children = 0" )
lin_hyp_4

#Question h. Estimate the regression model
# wkswork = α + β1chlt5 + β2Northeast + β3Midwest + β4South
# + γ1chlt5 × Northeast + γ2chlt5 × Midwest + γ3chlt5 × South + ε.

region <- relevel(region, ref="West") #Setting West as baseline

d5 <- lm(wkswork ~ chlt5*(region))
d5