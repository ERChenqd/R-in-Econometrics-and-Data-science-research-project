rm(list = ls())

library(tseries)
install.packages("urca")
library(urca)
install.packages("forecast")
library(forecast)
library(ggplot2)
library(dplyr)
library(zoo)
library(xts)
install.packages("ggfortify")
library(ggfortify)
install.packages("dyn")
library(dyn)
library(lmtest)
library(car)
library(readxl)
install.packages("ARDL")
library(ARDL)
library(dynlm)
library(tidyr)

#a Combine the datasets and construct the natural log
#format the datasets for subsequent analysis
QGDP=read_excel("C:/Users/24333/Downloads/GDPC1.xlsx")
QEMP=read_excel("C:/Users/24333/Downloads/PAYEMS.xlsx",sheet = "Sheet1")
QCAP=read_excel("C:/Users/24333/Downloads/RKNANPUSA666NRUG.xlsx",sheet = "Sheet1")
colnames(QGDP)=c("date", "gdp")
colnames(QEMP)=c("date","emp")
colnames(QCAP)=c("date","cap")
QGDP1=QGDP %>% slice(-(1:10))
QEMP1=QEMP %>% slice(-(1:10))
QCAP1=QCAP %>% slice(-(1:10))

#create data series data
date1=seq(as.Date("1975-01-01"), as.Date("2018-12-31"), by = "quarters")
print(date1)
QCAP1$date <- as.numeric(as.character(QCAP1$date))
QEMP1$date <- as.numeric(as.character(QEMP1$date))
QGDP1$date <- as.numeric(as.character(QGDP1$date))
QCAP1$date <- as.Date(QCAP1$date, origin = "1899-12-30")
QEMP1$date <- as.Date(QEMP1$date, origin = "1899-12-30")
QGDP1$date <- as.Date(QGDP1$date, origin = "1899-12-30")

QCAP_filtered=QCAP1 %>%
  filter(date %in% as.Date(date1))
QEMP_filtered=QEMP1 %>%
  filter(date %in% as.Date(date1))
QGDP_filtered=QGDP1 %>%
  filter(date %in% as.Date(date1))

Qdata_combined=cbind(QGDP_filtered, QEMP_filtered,QCAP_filtered)
Qdata_combined=Qdata_combined[, -c(3, 5)]

Qdata_combined$gdp=as.numeric(Qdata_combined$gdp)
Qdata_combined$cap=as.numeric(Qdata_combined$cap)
Qdata_combined$emp=as.numeric(Qdata_combined$emp)
Qdata_combined$lgdp=log(Qdata_combined$gdp)
Qdata_combined$lcap=log(Qdata_combined$cap)
Qdata_combined$lemp=log(Qdata_combined$emp)

Qdata_combined$diff_ln_gdp=c(NA, diff(Qdata_combined$lgdp, differences = 1))
Qdata_combined$diff_ln_emp=c(NA, diff(Qdata_combined$lemp, differences = 1))
Qdata_combined$diff_ln_cap=c(NA, diff(Qdata_combined$lcap, differences = 1))
Qdata_combined=Qdata_combined[-1, ]

#b Estimate the long run model
model1=lm(lgdp~lemp+lcap,data = Qdata_combined)
model1

#c
residualc=residuals(model1)
pacf(residualc) #the residual1 is an AR(2) process

ggplot(subset(Qdata_combined, date>="1975-01-01" & date<="2018-10-01"),
       aes(x = date, y = residualc)) +
  geom_line(aes(color = "residualc")) +
  labs(x = "Date", y = "residuals", color = "") +
  scale_color_manual(values = c("red")) +
  theme(legend.position="bottom") +
  ggtitle("Plot of residualc over time")

#ADF test
summary(ur.df(residualc, type="drift", lags = 2))
#Critical value of rejecting H0 at 5% significant level is -3.7429-8.352/176-13.41/176^2=-3.79
#Critical value of rejecting H0 at 1% significant level is -4.2981-13.790/176-46.37/176^2=-4.38
#Reject the H0 at 5% level, the series is non-stationary

#ERS test
summary(ur.ers(residualc, type= "DF-GLS", model = "constant", lag.max = 2))
#critical value at 10% significant level is -1.624 and -1.948 at 5% significant level
#Reject the H0 at any significant level, the series can be stationary

#KPSS test
summary(ur.kpss(residualc, type= c("mu"), use.lag = 2))
#Do not reject the H0 at 5% level, in which the series is stationary

#In summation, the residual can be I(0),there is a co-integration equation

#d
dyn1=dynlm(lgdp~lemp+lag(lemp,1)+lcap+lag(lcap,1)+lag(lgdp,1),data=Qdata_combined)
dyn1
Vector=coef(dyn1)

#In the long run, lgdpt=lgdp(t-1) etc therefore we could directly put the variables at different lags on the same side.
residuald=Qdata_combined$lgdp-(Vector[1]+(Vector[2]+Vector[3])*Qdata_combined$lemp+(Vector[4]+Vector[5])*Qdata_combined$lcap)/(1-Vector[6])

#ADF test
ggplot(subset(Qdata_combined, date>="1975-01-01" & date<="2018-10-01"),
       aes(x = date, y = residuald)) +
  geom_line(aes(color = "residuald")) +
  labs(x = "Date", y = "residuals", color = "") +
  scale_color_manual(values = c("red")) +
  theme(legend.position="bottom") +
  ggtitle("Plot of residuald over time")
#seems like model C
acf(residuald)
pacf(residuald)#AR(2)

summary(ur.df(residuald, type="trend", lags = 2))
#Do not reject the H0, the series is non-stationary, and the equation is spurious
#There is no co-integration equation

summary(ur.df(diff(residuald,differences = 1), type="drift", lags = 2))
#Reject the H0, the series is stationary, the equation is I(1)

#e
ECMe <- dyn$lm(diff_ln_gdp ~ lag(diff_ln_gdp, 1) + lag(diff_ln_emp, 1) +   
                lag(diff_ln_cap, 1) + lag(residualc, 1),
                data = Qdata_combined)
ECMe
#the coefficient on the error correction term is negative(-0.085), matching our expectation:
#the GDP growth is an endogenous variable which influenced inside the system being affected by capital and empployment
#, maintaining an equilibrium in GDP growth rate (balanced growth path) over long run.
#We could suggest a negative correlation between the output growth rate and output gap, that makes sense in real business cycles in Macroeconomics.
bpteste <- bptest(ECMe, data = Qdata_combined)
print(bpteste)
# 0.002203 < 0.05, reject null, heteroscedasticity present

resete <- resettest(ECMe, data = Qdata_combined)
print(resete)

bgtest_e=bgtest(ECMe,data=Qdata_combined)
bgtest_e

#f
ECMf <- dyn$lm(diff_ln_emp ~ lag(diff_ln_gdp, 1) + lag(diff_ln_emp, 1) +   
                lag(diff_ln_cap, 1) + lag(residualc, 1),
              data = Qdata_combined)
ECMf
#Coefficient on the error correction term is positive (0.0122).
#The coefficient suggested that there is a positive correlation between the output gap and the change in employment rate
#That makes sense in the business cycle, if the economy is a positive output gap, the employment rate would also be positive,
#being justified by the counter-cyclicality of unemployment.
bptestf <- bptest(ECMf, data = Qdata_combined)
print(bptestf)
# 0.002203 < 0.05, reject null, heteroscedasticity present

resetf <- resettest(ECMf, data = Qdata_combined)
print(resetf)

bgtest_f=bgtest(ECMf,data=Qdata_combined)
bgtest_f

#g
ECMg <- dyn$lm(diff_ln_cap ~ lag(diff_ln_gdp, 1) + lag(diff_ln_emp, 1) +   
                 lag(diff_ln_cap, 1) + lag(residualc, 1),
               data = Qdata_combined)
ECMg
#The coefficient on error correction term is -3.628e-03, which is -3.628*10^(-3)
#The correlation is negative but it is very small, 
#demonstrating a very weak correlation between the output gap and the capital growth
#Therefore the capital accumulation can be acyclical with the business cycles.
#The capital accumulation can be therefore exogenous, being affected by variables outside the model.
bptestg <- bptest(ECMg, data = Qdata_combined)
print(bptestg)
# 0.002203 < 0.05, reject null, heteroscedasticity present

resetg <- resettest(ECMg, data = Qdata_combined)
print(resetg)

bgtest_g=bgtest(ECMg,data=Qdata_combined)
bgtest_g

#we can use the hypothesis testing methods in term 1 to test the time series correlation in error term because we do not care about the lags and dynamic relations!