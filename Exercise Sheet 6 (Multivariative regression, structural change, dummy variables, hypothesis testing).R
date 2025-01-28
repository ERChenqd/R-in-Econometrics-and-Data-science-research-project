### Exercise Sheet 6
### Group 026

rm(list=ls())

# load('march_cps_1976.Rdata')

library(tidyverse)
library(car)
library(lmtest)

df <- data.frame(cps) # best practice of creating a separate dataframe to work with

### Question 1
model1_1 <- lm(wkswork ~ age + chlt5, data=df)
# summary(model1_1)

model1_2 <- lm(wkswork ~ chlt5, data=df)
# summary(model1_2)

model1_3 <- lm(age ~ chlt5, data=df)
# summary(model1_3)

res1_2 <- residuals(model1_2)
res1_3 <- residuals(model1_3)

model1_4 <- lm(res1_2 ~ res1_3, data=df)
# summary(model1_4)

b1 <- coef(summary(model1_1))[2,1]
a1 <- coef(summary(model1_4))[2,1]
b1
a1
# b1 and a1 are equal, measuring how much the age affect on wkswork
#if b1 not equal to a1, there is Multicolinear and cofounding between age and chlt5, beta 1 is not accurately isolating from age effect on wkswork

### Question 2
modelNortheast <- lm(wkswork ~ age + chlt5, data=subset(df, region=='Northeast'))
modelWest <- lm(wkswork ~ age + chlt5, data=subset(df, region=='West'))
modelMidwest <- lm(wkswork ~ age + chlt5, data=subset(df, region=='Midwest'))
modelSouth <- lm(wkswork ~ age + chlt5, data=subset(df, region=='South'))

# modelNortheast
# modelWest
# modelMidwest
# modelSouth

Northeast_RSS <- sum(residuals(modelNortheast)^2)
West_RSS <- sum(residuals(modelWest)^2)
Midwest_RSS <- sum(residuals(modelMidwest)^2)
South_RSS <- sum(residuals(modelSouth)^2)

Unrestrict_RSS <- sum(Northeast_RSS, West_RSS, Midwest_RSS, South_RSS) #the unrestricted model is a collection of the 4 models, the RSSu is the sum of these sub-RSS like the F-test and structural change
# Unrestrict_RSS

Unrestrict_n <- nrow(df) # = 32147
Unrestrict_param <- 3+3+3+3 # = 12



### Question 3
levels(df$region)
region=relevel(df$region,"Northeast")
model3 <- lm(wkswork ~ age+chlt5+age:region, data=df) #restricted model
# summary(model3)
 
Restrict_RSS <- sum(residuals(model3)^2)
# Restrict_RSS

Restrict_param <- 6

DoF = Unrestrict_n - Unrestrict_param # = 32135
d = Unrestrict_param - Restrict_param # = 6 (3 for multiplicative chlt5:region + 3 additive region dummy variable)

F = (Restrict_RSS-Unrestrict_RSS)/d/(Unrestrict_RSS/DoF)
F # F = 10.535 in 3 decimal places

# The restrictions are 3 for multiplicative chlt5:region + 3 additive region dummy variable:
# i.e. regionSouth:chlt5No small children = regionWest:chlt5No small children = regionMidwest:chlt5No small children = South = West = Midwest = 0
 

### Question 4
model4 <- lm(wkswork ~ age*region + chlt5*region, data=df)
#summary(model4)

# H0: ... = 0; H1: ... ≠ 0 (see below for ...)
linearHypothesis(model4,c("regionMidwest=0","regionSouth=0","regionWest=0",
                           "regionMidwest:chlt5No small children=0",
                           "regionSouth:chlt5No small children=0",
                           "regionWest:chlt5No small children=0")) #the first line of the H0 are hypothesizing the regional intercepts are zero

# For alpha of 5% we reject the Null Hypothesis since Pr(>F)<alpha
# F = 10.535

### Conclusion
# Question 3 value of F equals Question 4 value of F as expected