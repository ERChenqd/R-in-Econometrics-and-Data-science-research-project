###Exercise sheet 8
###Group 026

rm(list=ls())

#load("condra_long_shaver_wright_2018.RData")

df=data.frame(condra.etal)

library(tidyverse)
library(car)
library(lmtest)
library(estimatr)
library(tseries)
library(margins)
library(ivreg)
library(sandwich)

###Question 1

#(a)
# find the equation (1)
model1.1=lm(total_v2_agcho10~ df_5to11 + first + temp_00Z + temp_06Z +
            rain_00Z + rain_06Z, data = df)
summary(model1.1)
res1 = resid(model1.1) #the residuals from equation (1)
fity1 = fitted(model1.1) #the fitted values from equation (1)

#RESET(4) test:
model1.2 <- lm(res1 ~ df_5to11+first+temp_00Z+temp_06Z +
               +rain_00Z+rain_06Z+I(fity1^2)+I(fity1^3)+I(fity1^4), data=df)

reset_test1 <- linearHypothesis(model1.2, c("I(fity1^2)=0","I(fity1^3)=0","I(fity1^4)=0"))

fstat_reset1 <- reset_test1$F[2]
fstat_reset1 # 0.856

#(b)
model2 <- lm(total_v2_agcho10~ df_5to11+first+temp_00Z+temp_06Z +
               +rain_00Z+rain_06Z+I(fity1^2)+I(fity1^3)+I(fity1^4), data=df)

reset_test2 <- linearHypothesis(model2, c("I(fity1^2)=0","I(fity1^3)=0","I(fity1^4)=0"))

fstat_reset2 <- reset_test2$F[2]
fstat_reset2 # 0.856

#(c)
model3=lm(res1^2~df_5to11 + first + temp_00Z + temp_06Z +rain_00Z + rain_06Z+
                 I(df_5to11^2) + I(temp_00Z^2) + I(temp_06Z^2) +I(rain_00Z^2) + I(rain_06Z^2)
                 ,data=df)
Whitetest_c = nobs(model3)*summary(model3)$r.squared
Whitetest_c # 8.48

#Test joint significance of explanatory variables in model3: 
Jointtest_c=linearHypothesis(model3, c("df_5to11=0","first=0","temp_00Z=0","temp_06Z=0","rain_00Z=0","rain_06Z=0","I(df_5to11^2)=0","I(temp_00Z^2)=0", "I(temp_06Z^2)=0","I(rain_00Z^2)=0","I(rain_06Z^2)=0"))
Jointtest_c 

#(d) 
#Heteroscedasticity robust standard errors
model4=vcovHC(model1.1,type="HC3")
sqrt(diag(model4))

#OLS standard errors
summary(model1.1)$coefficients[, "Std. Error"]

#They have different standard errors but I think they are very close:
#> sqrt(diag(model4))
#(Intercept)    df_5to11       first    temp_00Z    temp_06Z    rain_00Z 
#0.593725286 0.006354123 0.042364749 0.004015891 0.003557850 0.002314996 
#rain_06Z 
#0.003906503 
#> #OLS standard errors
 # > summary(model1.1)$coefficients[, "Std. Error"]
#(Intercept)    df_5to11       first    temp_00Z    temp_06Z    rain_00Z 
#0.699852705 0.007983929 0.053608608 0.003556098 0.002842878 0.003913219 
#rain_06Z 
#0.004515706 


###Question 2

#(a)
formula_iv <- total_v2_agcho10 ~ df_5to11 + windspeed_06Z + windspeed_12Z +
  I(windspeed_06Z^2) + I(windspeed_12Z^2) +
  temp_00Z + temp_06Z + temp_12Z +
  rain_00Z + rain_06Z + rain_12Z +
  I(temp_00Z^2) + I(temp_06Z^2) + I(temp_12Z^2) +
  I(rain_00Z^2) + I(rain_06Z^2) + I(rain_12Z^2) +
  population_2010_adj |
  plus_wind_00Z_10 + windspeed_06Z + windspeed_12Z +
  I(windspeed_06Z^2) + I(windspeed_12Z^2) +
  temp_00Z + temp_06Z + temp_12Z +
  rain_00Z + rain_06Z + rain_12Z +
  I(temp_00Z^2) + I(temp_06Z^2) + I(temp_12Z^2) +
  I(rain_00Z^2) + I(rain_06Z^2) + I(rain_12Z^2) +
  population_2010_adj

model2a <- ivreg(formula_iv, data = df)
model2a

#(b)
model2b=lm(total_v2_agcho10~df_5to11+windspeed_06Z+windspeed_12Z+I(windspeed_06Z^2)+I(windspeed_12Z^2)+
             temp_00Z+temp_06Z+temp_12Z+rain_00Z+rain_06Z+rain_12Z+I(temp_00Z^2)+I(temp_06Z^2)+I(temp_12Z^2)+I(rain_00Z^2)+I(rain_06Z^2)+I(rain_12Z^2)+
             population_2010_adj, data=df)
model2b

#(c) Hausman-Wu test for endogeneity of df_5to11:

#regress df_5to11 on its instrument and exogenous variables
model2c1=lm(df_5to11~plus_wind_00Z_10+windspeed_06Z+windspeed_12Z+I(windspeed_06Z^2)+I(windspeed_12Z^2)+
             temp_00Z+temp_06Z+temp_12Z+rain_00Z+rain_06Z+rain_12Z+I(temp_00Z^2)+I(temp_06Z^2)+I(temp_12Z^2)+I(rain_00Z^2)+I(rain_06Z^2)+I(rain_12Z^2)+
             population_2010_adj, data=df)
res2c=resid(model2c1) #extract the residual of the model

model2c2=lm(total_v2_agcho10~df_5to11+windspeed_06Z+windspeed_12Z+I(windspeed_06Z^2)+I(windspeed_12Z^2)+
             temp_00Z+temp_06Z+temp_12Z+rain_00Z+rain_06Z+rain_12Z+I(temp_00Z^2)+I(temp_06Z^2)+I(temp_12Z^2)+I(rain_00Z^2)+I(rain_06Z^2)+I(rain_12Z^2)+
             population_2010_adj+res2c, data=df)
linearHypothesis(model2c2,"res2c=0")
#Test statistic is 4.1733, P-value is 0.042, we should reject the H0 at 5% significant level.
#Therefore, it is statistically significant to say that df_5to11 is endogenous, 
#containing information on explaining total_v2_agcho10.