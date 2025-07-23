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

#(a)
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
date1=seq(as.Date("1975-04-01"), as.Date("2019-01-01"), by = "quarters")
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

#a.1. combine 3 datasets
Qdata_combined=cbind(QGDP_filtered, QEMP_filtered,QCAP_filtered)
Qdata_combined=Qdata_combined[, -c(3, 5)]
Qdata_combined

#a.2. construct the natural log
Qdata_combined$gdp=as.numeric(Qdata_combined$gdp)
Qdata_combined$cap=as.numeric(Qdata_combined$cap)
Qdata_combined$emp=as.numeric(Qdata_combined$emp)
Qdata_combined$lgdp=log(Qdata_combined$gdp)
Qdata_combined$lcap=log(Qdata_combined$cap)
Qdata_combined$lemp=log(Qdata_combined$emp)

#(b)  Plot time series on separate graphs
Figure1=plot(date1, Qdata_combined$lgdp, type = "l", col = "blue", lwd = 2, xlab = "Date", ylab = "log gdp", main = "Log GDP")
Figure2=plot(date1, Qdata_combined$lemp, type = "l", col = "red", lwd = 2, xlab = "Date", ylab = "log emp", main = "Log Employment")
Figure3=plot(date1, Qdata_combined$lcap, type = "l", col = "green", lwd = 2, xlab = "Date", ylab = "log cap", main = "Log Capital stock")
#The 3 series are all increasing in time and they are all positively correlated to each other.
#log employment is more volatile than log GDP, log capital stock is the least volatile.

#A summary graph
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

#(c) Plot the ACF and PACF of the first difference of ln(gdp), ln(emp) and ln(cap)
#C.1. find the first differences
Qdata_combined$diff_ln_gdp=c(NA, diff(Qdata_combined$lgdp, differences = 1))
Qdata_combined$diff_ln_emp=c(NA, diff(Qdata_combined$lemp, differences = 1))
Qdata_combined$diff_ln_cap=c(NA, diff(Qdata_combined$lcap, differences = 1))
Qdata_combined=Qdata_combined[-1, ]

#C.2. Plot the ACF and PACF
#ln(GDP)
par(mfrow = c(1, 2))  # Set plotting layout for side-by-side plots
acf(Qdata_combined$diff_ln_gdp, main = "ACF: I(1) of ln(GDP)", lag.max = 20)
pacf(Qdata_combined$diff_ln_gdp, main = "PACF: I(1) of ln(GDP)", lag.max = 20)

#ln(Employment)
par(mfrow = c(1, 2))
acf(Qdata_combined$diff_ln_emp, main = "ACF: I(1) of ln(Emp)", lag.max = 20)
pacf(Qdata_combined$diff_ln_emp, main = "PACF: I(1) of ln(Emp)", lag.max = 20)

#ln(Capital stock)
par(mfrow = c(1, 2))
acf(Qdata_combined$diff_ln_cap, main = "ACF: I(1) of ln(Capital)", lag.max = 20)
pacf(Qdata_combined$diff_ln_cap, main = "PACF: I(1) of ln(Capital)", lag.max = 20)

#C.3. Use the plots to identify the ARIMA processes which describe the 3 series:
#ln(GDP) cuts off at lag 1 in PACF and peters out, suggesting AR(1).It decays in ACF, suggesting MA(0)
#Hence ln(GDP) can be ARIMA(1,1,0).
#ln(Emp) gradually peters out in ACF and strikes at lag 1 in PACF, suggesting ARIMA(1,1,0)
#ln(Capital) gradually decays in ACF and strikes at lag 1 in PACF, suggesting ARIMA(1,1,0)

#(d) estimate the ARIMA models.
loopVec=list(c(0,1,0),c(1,1,0),c(2,1,0),c(0,1,1),c(1,1,1),c(2,1,1),c(0,1,2),c(1,1,2),c(2,1,2))
#ln(gdp)
adf.test(Qdata_combined$lgdp)
#P-value=0.7388,which does not reject the H0, the series is therefore non-stationary
for (loopitem in loopVec){
  ARIMA=Arima(Qdata_combined$lgdp, order = loopitem, include.constant=TRUE)
  print(ARIMA)
  checkresiduals(ARIMA)
}
#ln(emp)
adf.test(Qdata_combined$lemp)
#P-value=0.3443, which does not reject the H0, the series is therefore non-stationary
for (loopitem in loopVec){
  ARIMA=Arima(Qdata_combined$lemp, order = loopitem, include.constant=TRUE)
  print(ARIMA)
  checkresiduals(ARIMA)
}
#ln(cap)
adf.test(Qdata_combined$lcap)
#P-value=0.99, which does not reject the H0, the series is therefore non-stationary
for (loopitem in loopVec){
  ARIMA=Arima(Qdata_combined$lcap, order = loopitem, include.constant=TRUE)
  print(ARIMA)
  checkresiduals(ARIMA)
}

#(e) compare the AIC of the models, the model with the lowest AIC is the most appropriate one
#ln(gdp):ARIMA(1,1,0) is the most appropriate model, it also has the least standard error in ADF test.
#ln(emp)：ARIMA(1,1,0) is the most appropriate model
#ln(cap):ARIMA(1,1,0)

#(f)OLS model
modelf=lm(diff_ln_gdp~diff_ln_emp+diff_ln_cap,data=Qdata_combined)
summary(modelf)
#Alternatively,
modelf <- dynlm(d(lgdp) ~ d(lemp) + d(lcap), data=subset(data1, 
                                                         Index>="1974-10-01" & 
                                                           Index<="2018-10-01"))
summary(model2)

#(g)ACF and PACF of residuals
residualf=residuals(modelf)
residualPlot(modelf)
par(mfrow = c(1, 2))  # Set up a two-panel plot
acf(residualf, main = "ACF of Residuals")
pacf(residualf, main = "PACF of Residuals")
#No serial correlation in residuals because there is no significant ACF strikes at any lag.

#(h)BG test of 4th order serial correlation in the residuals in modelf
bgtest(modelf,order=4,data=Qdata_combined)
#P-value=0.76, reject H0, no serial correlation

#(i)Auxiliary regression and test the BG test
residualf=residuals(modelf)
modeli=dyn$lm(residualf~lag(residualf,1)+lag(residualf,2)+lag(residualf,3)+lag(residualf,4)+diff_ln_emp+diff_ln_cap, data=Qdata_combined)
linearHypothesis(modeli, c("lag(residualf, 1)=0", 
                           "lag(residualf, 2)=0", 
                           "lag(residualf, 3)=0", 
                           "lag(residualf, 4)=0"))
#F=0.41, P-value=0.80

#(j)Estimate the ADL model
#modelj=dyn$lm(diff_ln_gdp~diff_ln_emp+lag(diff_ln_emp,1)+lag(diff_ln_emp,2)+lag(diff_ln_emp,3)+
#                diff_ln_cap+lag(diff_ln_cap,1)+lag(diff_ln_cap,2)+lag(diff_ln_cap,3), data=Qdata_combined)
#modelj=auto_ardl(diff_ln_gdp~ diff_ln_emp+diff_ln_cap, 
#                   data = Qdata_combined, 
#                   max_order = c(4,4,4),  # Set max lags for dependent and independent variables
#                   selection = "AIC")
#modelj
#long_run_response=sum(coef(modelj)[grep("diff_ln_emp", names(coef(modelj)))])
#long_run_response
#long run response: 0.40

# Define maximum lag to test (e.g., 4 lags)
max_lag <- 4

# Initialize variables to track the best model
best_aic <- Inf
best_p <- 0
best_q <- 0

# Loop through all combinations of p (for employment) and q (for capital)
for (p in 0:max_lag) {
  for (q in 0:max_lag) {
    # Skip if no lags for both variables
    if (p == 0 & q == 0) next
    
    # Create formula dynamically
    formulaJ <- as.formula(
      paste("Qdata_combined$diff_ln_gdp ~ ",
            ifelse(p >= 0, paste("L(Qdata_combined$diff_ln_emp, 0:", p, ")", sep = ""), ""),
            "+",
            ifelse(q >= 0, paste("L(Qdata_combined$diff_ln_cap, 0:", q, ")", sep = ""), "")
      )
    )
    
    # Estimate the model
    modelj <- try(dynlm(formulaJ, data = Qdata_combined), silent = TRUE)
    
    # Skip if model fails (e.g., singular matrix)
    if (inherits(modelj, "try-error")) next
    
    # Calculate AIC
    current_aic <- AIC(modelj)
    
    # Update best model if current AIC is lower
    if (current_aic < best_aic) {
      best_aic <- current_aic
      best_p <- p
      best_q <- q
    }
  }
}

# Re-estimate the best model
final_modelj <- dynlm(
  diff_ln_gdp ~ L(diff_ln_emp, 0:best_p) + L(diff_ln_cap, 0:best_q),
  data = Qdata_combined
)

emp_coefs <- coef(final_modelj)[grepl("diff_ln_emp", names(coef(final_modelj)))]
long_run_responseJ <- sum(emp_coefs)
long_run_responseJ
#long_run_response=0.801

#(k)BG test of 4th order serial correlation in the residuals of modelj
bgtest(final_modelj,order=4,data=Qdata_combined)
#P-value=0.7632

#(L)A model with lags of explanatory variables and lags of the independent variable
#modelL=dyn$lm(diff_ln_gdp~diff_ln_emp+lag(diff_ln_emp,1)+lag(diff_ln_emp,2)+lag(diff_ln_emp,3)+
#                diff_ln_cap+lag(diff_ln_cap,1)+lag(diff_ln_cap,2)+lag(diff_ln_cap,3)+
#                lag(diff_ln_gdp,1)+lag(diff_ln_gdp,2)+lag(diff_ln_gdp,3),
#              data=Qdata_combined)
#modelL

# Define maximum lag to test (e.g., 4 lags)
max_lag <- 4

# Initialize variables to track the best model
best_aic <- Inf
best_p <- 0
best_n <- 1
best_q <- 0

# Loop through all combinations of p (for employment) and q (for capital)
for (n in 0:max_lag) {
  for (p in 0:max_lag) {
    for (q in 0:max_lag) {
    # Skip if no lags for both variables
      if (p == 0 & q == 0) next
    
    # Create formula dynamically
      formulaL <- as.formula(
        paste("Qdata_combined$diff_ln_gdp ~ ",
              ifelse(p >= 0, paste("L(Qdata_combined$diff_ln_emp, 0:", p, ")", sep = ""), ""),
              "+",
              ifelse(q >= 0, paste("L(Qdata_combined$diff_ln_cap, 0:", q, ")", sep = ""), ""),
              "+",
              ifelse(n >= 0, paste("L(Qdata_combined$diff_ln_gdp, 0:", n, ")", sep = ""), "")
        )
      )
    
      # Estimate the model
      modelL <- try(dynlm(formulaL, data = Qdata_combined), silent = TRUE)
    
      # Skip if model fails (e.g., singular matrix)
      if (inherits(modelL, "try-error")) next
    
      # Calculate AIC
      current_aic <- AIC(modelL)
    
      # Update best model if current AIC is lower
      if (current_aic < best_aic) {
        best_aic <- current_aic
        best_p <- p
        best_q <- q
      }
    }
 }
}
# Re-estimate the best model
final_modelL <- dynlm(
  diff_ln_gdp ~ L(diff_ln_emp, 0:best_p) + L(diff_ln_cap, 0:best_q)+ L(diff_ln_gdp, 0:best_q),
  data = Qdata_combined
)

emp_coefs <- coef(final_modelL)[grepl("diff_ln_emp", names(coef(final_modelL)))]
long_run_responseL <- sum(emp_coefs)
long_run_responseL
#Long run response=2.792339e-16

#(M) BD test of 4th order serial correlation in the residuals of the modelL
bgtest(final_modelL,order=4,data=Qdata_combined)
#P-value=0.6893