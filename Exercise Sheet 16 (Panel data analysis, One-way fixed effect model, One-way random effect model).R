library(readxl)
library(ggplot2)
library(dplyr)
library(lmtest)
library(haven)
library(Hmisc)
library(car)
library(MASS)
library(sandwich)
library(stargazer)
library(plm)

setwd("C:/Users/karlo/OneDrive/Documentos/1 Carpetas/PhD/Metrics 2/PS 16")

pwt91a <- read_dta("pwt91a.dta")

##1(a): Plot lnGDP for 5 countries: USA, UK, Germany, China and India
ggplot(data = subset(pwt91a, countrycode %in% c("USA","GBR","DEU","CHN","IND")), aes(x = year, y = lngdp, colour = countrycode)) + 
  geom_line()

# The economies of USA, UK and Germany have witnessed a steady growth over time.
# China and India seem to be catching-up to USA’s GDP. Note that GDP is measured in
# chained PPP 2011 USD.

#1(b): pooled ols
pols_1 <- lm(lngdp ~ lnL + lnK + lnL2 +lnK2 + hc + as.factor(year), data = pwt91a)
summary(pols_1)

# A unit increase in human capital increased gdp by around 100*[exp(0.182)-1]=20%.

#1(c): plot the implied relationship between lngdp and lnL 
pwt91a$ols_fit <- with(pwt91a, pols_1$coefficients[1] + pols_1$coefficients[2] * lnL + 
                         pols_1$coefficients[4] * lnL2)

ggplot(data = subset(pwt91a, countrycode == "USA"), aes(x=lnL, y = ols_fit)) +
  geom_line()

#this effect is near linear and this should be unsurprising given the size of the coefficient on squared ln(L)

##1(d): clustering SE at the country level
#help(vcovCL)
cov1 <- vcovCL(pols_1, 
               type = "HC1",
               cluster = ~ countrycode)
robust_se <- sqrt(diag(cov1))

stargazer(pols_1, pols_1, type="text",
          se = list(NULL, robust_se),
          keep = c("lnL", "lnK", "lnL2", "lnK2", "hc"))

##1(e)
# Standard errors are markedly bigger in the clustered effects, suggesting OLS over-estimated
# the significance of the variables (as there is some form of correlation)

##1(f): FE model
help(plm)
#for FE: model = "within"

fe <- plm(lngdp ~ lnL + lnK + lnL2 + lnK2 + hc + year,
                data = pwt91a,
                index = c("countrycode", "year"),
                model = "within")
summary(fe)

# Coefficient on hc is even larger than in OLS case and would suggest an increase in hc
# within a country by 1 would increase gdp by 21%, but remember the within sd of hc is
# only 0.18 and therefore a unit change is massive.

##1(g): RE model
# RE:  model = "random"
re <- plm(lngdp ~ lnL + lnK + lnL2 + lnK2 + hc + year,
              data = pwt91a,
              index = c("countrycode", "year"),
              model = "random")
summary(re)

# Coefficient on hc is even larger than in OLS case and would suggest an increase in hc
# within a country by 1 would increase gdp by 40%, but remember the within sd of hc is
# only 0.18 and therefore a unit change is massive.

##1(h):plot the implied relationship between lngdp and lnL using FE estimates
pwt91a$fe_fit <- fe$coefficients[1] + fe$coefficients[2] * pwt91a$lnL + 
  fe$coefficients[4] * pwt91a$lnL2

ggplot(data = subset(pwt91a, countrycode == "USA"), aes(x=lnL, y = fe_fit)) +
  geom_line()

##1(i):model comparison
help(stargazer)

stargazer(pols_1, pols_1, fe, re, type="text",
          se = list(NULL, robust_se, NULL, NULL),
          keep = c("lnL", "lnK", "lnL2", "lnK2", "hc"))
#Pooled OLS, clustered S.E, Fixed effect, Random effect
##1(j): hausman test
phtest(fe, re)

#null: Random effects (RE) model is consistent.
#alternative: FE model is preferred because RE is inconsistent.
#p-value = 1.097e-08 (< 0.05), so reject null
