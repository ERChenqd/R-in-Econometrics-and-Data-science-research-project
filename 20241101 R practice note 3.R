#correlation
cor(qlfs$hpay,qlfs$edage,use="complete.obs") #only use the complete observations
# it measures the linear relationship, correlation=0 means no "linear" correlation, not necessarily means no correlation

#two variable regression
reg.out=lm(hpay~edage,data=qlfs)
sum.reg=summary(reg.out)
sum.reg #the P-value is too small, less than S.L, hence reject H0. The t value is also bigger than critical value.
#the one unit change in edage will increase the hpay by 0.7639

#Test hypothesis on beta=0.8
install.packages("car")
library(car)
linearHypothesis(reg.out,"edage=0.8")

#calculate the power, true beta=0.75, S.L=5%, 
#the prob of rejecting the H0 given H0 is False
coef(sum.reg)
standard.error=coef(summary(reg.out))[2,2]
#[2,2] accesses the second row, second column of this matrix:
#The first index (2) specifies the row, which corresponds to the second coefficient in the model (often the slope in a simple linear regression).
#The second index (2) specifies the column, which corresponds to the standard error.
standard.error
power1=1-pnorm(1.96+(0.8-0.75)/standard.error)
power2=pnorm(-1.96+(0.8-0.75)/standard.error)
power=power1+power2
power

#R squared, goodness of fit, is the square of the correlation in the two variable regression model
rho_sqrd=cor(qlfs$hpay,qlfs$edage,use="complete.obs")^2
R2=sum.reg$r.squared

rho_sqrd
R2

#semi-elasticity
coef(sum.reg)[2,1]/mean(qlfs$hpay,na.rm=TRUE)
# 5.23% increase in hpay in relative to unit change in edage

#new model
reg.out2=lm(lhpay~edage,data=qlfs)
summary(reg.out2)
#the average of hourly pay increase by 100*(exp(beta)-1)%

# semi-elasticity in this case is coefficient beta
reg.out2$coefficients[2]
coef(sum.reg)[2,1]/mean(qlfs$hpay,na.rm=TRUE)
# the elasticity estimates are close.

#
qlfs$agesq=qlfs$age^2
reg.out3=lm(lhpay~age+agesq,data=qlfs)
summary(reg.out3)
