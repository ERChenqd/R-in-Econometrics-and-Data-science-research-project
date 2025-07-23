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

#1a Combine the datasets and construct the natural log
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

Qdata_combined

#1b ACF and PACF plots
par(mfrow = c(1, 2))  # Set plotting layout for side-by-side plots
acf(Qdata_combined$lgdp, main = "ACF: ln(GDP)", lag.max = 20)
pacf(Qdata_combined$lgdp, main = "PACF: ln(GDP)", lag.max = 20)

acf(Qdata_combined$lemp, main = "ACF: ln(Emp)", lag.max = 20)
pacf(Qdata_combined$lemp, main = "PACF: ln(Emp)", lag.max = 20)

acf(Qdata_combined$lcap, main = "ACF: ln(Capital)", lag.max = 20)
pacf(Qdata_combined$lcap, main = "PACF: ln(Capital)", lag.max = 20)
#All the variables are non-stationary and AR(1) processes.

ggplot(subset(Qdata_combined, date>="1975-01-01" & date<="2018-10-01"),
       aes(x = date, y = lgdp)) +
  geom_line(aes(color = "ln(GDP)")) +
  geom_line(aes(y = lemp-2, color = "ln(EMP)")) +
  geom_line(aes(y = lcap-8, color = "ln(CAP)")) +
  scale_y_continuous(sec.axis = sec_axis(~.+ 8, name="ln(cap)")) +
  labs(x = "Date", y = "ln(GDP)", color = "") +
  scale_color_manual(values = c("red", "blue", "green")) +
  theme(legend.position="bottom") +
  ggtitle("Plot of ln(GDP), ln(EMP) and ln(CAP) over time")

#I(1):
par(mfrow = c(1, 2))
acf(diff(Qdata_combined$lgdp, differences = 1), main = "ACF: I(1)ln(GDP)", lag.max = 20)
pacf(diff(Qdata_combined$lgdp, differences = 1), main = "PACF: I(1) ln(GDP)", lag.max = 20)
#Accordingly, we could suggest that lgdp is in the 1st order of integration, AR(1)

par(mfrow = c(1, 2))
acf(diff(Qdata_combined$lemp, differences = 1), main = "ACF: I(1)ln(Emp)", lag.max = 20)
pacf(diff(Qdata_combined$lemp, differences = 1), main = "PACF: I(1) ln(Emp)", lag.max = 20)
#Accordingly, we could suggest that lemp is in the 1st order of integration, AR(1)

par(mfrow = c(1, 2))
acf(diff(Qdata_combined$lcap, differences = 1), main = "ACF: I(1)ln(Cap)", lag.max = 20)
pacf(diff(Qdata_combined$lcap, differences = 1), main = "PACF: I(1) ln(Cap)", lag.max = 20)

#I(2):
acf(diff(Qdata_combined$lcap, differences = 2), main = "ACF: I(2)ln(Cap)", lag.max = 20)
pacf(diff(Qdata_combined$lcap, differences = 2), main = "PACF: I(2) ln(Cap)", lag.max = 20)
#Accordingly, we could suggest that lcap is in the 2nd order of integration

#1c ADF test for the 3 series, a small power test
#According to the plot of ln(GDP),ln(EMP) and ln(Cap) over time, 
#I would use the ADF test model C and respective lags for each series.
summary(ur.df(Qdata_combined$lgdp, type="trend", lags = 1))
#Do not reject the H0 at any significant level
summary(ur.df(Qdata_combined$lcap, type="trend", lags = 1))
#Do not reject the H0 at any significant level
summary(ur.df(Qdata_combined$lemp, type="trend", lags = 1))
#Do not reject the H0 at any significant level


summary(ur.df(diff(Qdata_combined$lgdp, differences = 1), type="drift", lags = 1))
#Reject the H0 at any significant level, it takes one difference to make the series stationary, 
#in which lgdp is the 1st order of integration
summary(ur.df(diff(Qdata_combined$lcap, differences = 1), type="drift", lags = 1))
#Do not reject the H0 at any significant level
summary(ur.df(diff(Qdata_combined$lemp, differences = 1), type="drift", lags = 1))
#Reject the H0 at any significant level, it takes one difference to make the series stationary, 
#in which lemp is the 1st order of integration

summary(ur.df(diff(Qdata_combined$lcap, differences = 2), type="drift", lags = 1))
#Reject the H0 at any significant level, it takes two differences to make the series stationary, 
#in which lcap is the 2nd order of integration

#1d: ERS test on the 3 series and determine the order of integration for each series, a bigger power test
summary(ur.ers(Qdata_combined$lgdp, type= "DF-GLS", model = "trend", lag.max = 1))
#Do not reject the H0 at any significant level
summary(ur.ers(Qdata_combined$lemp, type= "DF-GLS", model = "trend", lag.max = 1))
#Do not reject the H0 at any significant level
summary(ur.ers(Qdata_combined$lcap, type= "DF-GLS", model = "trend", lag.max = 1))
#Do not reject the H0 at any significant level

summary(ur.ers(diff(Qdata_combined$lgdp,differences = 1), type= "DF-GLS", model = "cons", lag.max = 1))
#Reject the H0 at any significant level,the series is therefore stationary at one difference and is the 1st order of integration
summary(ur.ers(diff(Qdata_combined$lemp,differences = 1), type= "DF-GLS", model = "cons", lag.max = 1))
#Reject the H0 at 10% significant level,the series is therefore stationary at one difference and is the 1st order of integration
summary(ur.ers(diff(Qdata_combined$lcap,differences = 1), type= "DF-GLS", model = "cons", lag.max = 1))
#Do not reject the H0 at any significant level

summary(ur.ers(diff(Qdata_combined$lcap,differences = 2), type= "DF-GLS", model = "cons", lag.max = 1))
#Reject the H0 at any significant level, the series is therefore stationary at two differences and is the 2nd order of integration

#1e:KPSS test on the 3 series lgdp, lemp and lcap, and determine the order of integration, a robust power test
summary(ur.kpss(Qdata_combined$lgdp, type= c("tau"), use.lag = 1))
#Reject the H0 at any significant level, the series is non-stationary
summary(ur.kpss(Qdata_combined$lemp, type= c("tau"), use.lag = 1))
#Reject the H0 at any significant level, the series is non-stationary
summary(ur.kpss(Qdata_combined$lcap, type= c("tau"), use.lag = 1))
#Reject the H0 at any significant level, the series is non-stationary

#Test the series again at the 1st difference
summary(ur.kpss(diff(Qdata_combined$lgdp,differences =1), type= c("tau"), use.lag = 1))
#Do not reject the H0 at any significance level, the series is stationary at the 1st order of integration
summary(ur.kpss(diff(Qdata_combined$lemp,differences =1), type= c("tau"), use.lag = 1))
#Do not reject the H0 at 5% significance level, the series is stationary at the 1st order of integration
summary(ur.kpss(diff(Qdata_combined$lcap,differences = 1), type= c("tau"), use.lag = 1))
#Reject the H0 at any significance level, the series is non-stationary

summary(ur.kpss(diff(Qdata_combined$lcap,differences = 2), type= c("tau"), use.lag = 1))
#Do not reject the H0 at any significance level, the series is stationary at the 2nd order of integration