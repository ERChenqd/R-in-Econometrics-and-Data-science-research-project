#First, load the data and check it:
rm(list = ls())
#setwd("CD:\陈尓文\华威大学\Economics（L100）\Notes\Second year\Econometrics\Group project")

#This is a Stata data: we need library(haven)
#the first time:
install.packages("haven") #remember the comand + write the "." (let it run)
library(haven)

ldv <- read_dta("condra_long_shaver_wright_2018.dta")
View(ldv)
summary(ldv)


#a) estimate the equation as a linear probability model
summary(ldv$total_v2_agcho10)

#create the dummy variable:
pr75 <- quantile(ldv$total_v2_agcho10, probs = 0.75)

ldv$pr75 <- pr75
dommy <- ldv$total_v2_agcho10 >= pr75
ldv$vote_h <- 0
ldv$vote_h[dommy] <- 1
#alternative:
ldv$vote_h <- ifelse(ldv$total_v2_agcho10 >= pr75, 1, 0)
table(ldv$vote_h)

#regression: 
model1 <- lm(vote_h ~ df_5to11 + first + temp_00Z + temp_06Z + rain_00Z + rain_06Z, data = ldv)
summary(model1)

#b)estimate the equation as a logit model
#Important: glm + family = binomial(link = "logit")
model2 <- glm(vote_h ~ df_5to11 + first + temp_00Z + temp_06Z + rain_00Z + rain_06Z, 
              data=ldv, family = binomial(link = "logit"))
summary(model2)


#c) calculate the AME of each variable in the equation
install.packages("margins")
library(margins)
margins(model2) #partially differentiate the equation with respect to each variable for individual data sets and calculate the mean effects for each 

#d) AME
margins(model1)
# compare the AME in the 2 different models

#e)
# Summarize the means of the variables
# first average values for all variables:df_5to11, temp_06Z, rain_00Z, rain_06Z
#without dplyr:
m_df_5to11 <- mean(ldv$df_5to11, na.rm = TRUE)
m_temp_06Z <- mean(ldv$temp_06Z, na.rm = TRUE)
m_rain_00Z <- mean(ldv$rain_00Z, na.rm = TRUE)
m_rain_06Z <- mean(ldv$rain_06Z, na.rm = TRUE)

# Create the data frame for prediction
predict_data <- data.frame(
  df_5to11 = c(m_df_5to11),
  first = c(1),
  temp_00Z = c(260, 264, 268, 272, 276, 280, 284, 288, 292, 296, 300),
  temp_06Z = c(m_temp_06Z),
  rain_00Z = c(m_rain_00Z),
  rain_06Z = c(m_rain_06Z)
)

summary(predict_data)
View(predict_data)
predict(model2, predict_data, type = "response")
#As the temp_00Z increases, the probability decreases.
#         1          2          3          4          5          6          7 
#0.49739212 0.41122972 0.33018762 0.25811499 0.19714390 0.14770772 0.10898544 
#8          9         10         11 
#0.07946788 0.05742956 0.04122925 0.02945608 

#f)
#Compute Lambda using fitted values from model2
ldv$Lambda <- fitted(model2)

#Compute ame1: the AME for temp_00Z using the mathematical expression
ldv$ame1 <- ldv$Lambda * (1 - ldv$Lambda) * model2$coefficients[4]

# Compute yhat1
ldv$yhat1 <- model2$coefficients[1] +
  model2$coefficients[2] * ldv$df_5to11 +
  model2$coefficients[4] * ldv$temp_00Z +
  model2$coefficients[5] * ldv$temp_06Z +
  model2$coefficients[6] * ldv$rain_00Z +
  model2$coefficients[7] * ldv$rain_06Z

# Compute yhat2
ldv$yhat2 <- ldv$yhat1 + model2$coefficients[3]

# Compute ame2: the AME for first using the mathematical expression
ldv$ame2 <- exp(ldv$yhat2) / (1 + exp(ldv$yhat2)) - exp(ldv$yhat1) / (1 + exp(ldv$yhat1))

summary(ldv$ame1)
summary(ldv$ame2)

# g), AME in probit model
#similar to 
#model2 <- glm(vote_h ~ df_5to11 + first + temp_00Z + temp_06Z
#             + rain_00Z + rain_06Z, data=ldv, family = binomial(link = "logit"))

model3 <- glm(vote_h ~ df_5to11 + first + temp_00Z + temp_06Z + rain_00Z + rain_06Z, 
              data=ldv, family = binomial(link = "probit"))
summary(model3)
margins(model3)

# h) similar to f:
#calculate the AMEs in probit model using mathematical expression

# Compute phi using fitted values from model3
ldv$phi <- fitted(model3)

# Compute n_ame1
#density
#help(dnorm)
#quantile
#help(qnorm)
# -> dnorm gives the density (pdf), pnorm gives the distribution function (cdf), qnorm gives the quantile function, and rnorm generates random deviates.

#modelling the derivative(pdf) of the Probit cdf model
ldv$n_ame1 <- dnorm(qnorm(ldv$phi)) * model3$coefficients[4]

# Compute n_yhat1
ldv$n_yhat1 <- model3$coefficients[1] +
  model3$coefficients[2] * ldv$df_5to11 +
  model3$coefficients[4] * ldv$temp_00Z +
  model3$coefficients[5] * ldv$temp_06Z +
  model3$coefficients[6] * ldv$rain_00Z +
  model3$coefficients[7] * ldv$rain_06Z

# Compute n_yhat2
ldv$n_yhat2 <- ldv$n_yhat1 + model3$coefficients[3]

# Compute n_ame2
# distribution:
help(pnorm)
ldv$n_ame2 <- pnorm(ldv$n_yhat2) - pnorm(ldv$n_yhat1)

summary(ldv$n_ame1)
summary(ldv$n_ame2)
