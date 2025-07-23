library(plm)
library(stargazer)
library(xtable)

load("C:/Users/24333/Downloads/pdat.Rdata")
View(pdat)

#POLS
model_2 = plm(d_voteshare~ dd_ln_income:lag(rep_win,4)+d_ln_enrollment+d_white_share,data = pdat)
summary(model_2)


#Regress on second-order income dynamics
model_1 = plm(d_voteshare~ dd_ln_income:factor(lag(rep_win,4))+d_ln_enrollment+factor(year)+d_white_share,data = pdat)
summary(model_1)

#second-order, including year in slope
model_3 = plm(d_voteshare~ dd_ln_income:factor(lag(rep_win,4)):factor(year)+d_ln_enrollment+factor(year)+d_white_share,data = pdat)
summary(model_3)

#second-order, controlling for red/swing/blue states
model_4 = plm(d_voteshare~ dd_ln_income:factor(lag(rep_win,4)):I(factor(class))+d_ln_enrollment+factor(year)+factor(class)+d_white_share,data = pdat)
summary(model_4)

stargazer(model_1,
          type = "latex",
          title = "Second-Order Income Dynamics Regression",
          label = "tab:model1",
          dep.var.labels = "Change in Republican Vote Share",
          no.space = TRUE,
          float = TRUE,
          float.env = "table")

stargazer(model_3,
          type = "latex",
          title = "Second-Order Income Dynamics with Year-Specific Slopes",
          label = "tab:model3",
          dep.var.labels = "Change in Republican Vote Share",
          no.space = TRUE,
          float = TRUE,
          float.env = "table")

stargazer(model_4,
          type = "latex",
          title = "Second-Order Income Dynamics with State Class Interactions",
          label = "tab:model4",
          dep.var.labels = "Change in Republican Vote Share",
          no.space = TRUE,
          float = TRUE,
          float.env = "table")

#F-tests
test1 <- pFtest(model_3, model_1)
test2 <- pFtest(model_4, model_1)

fstat1 <- test1$statistic
pval1 <- test1$p.value
fstat2 <- test2$statistic
pval2 <- test2$p.value

#Make a data frame
f_tests <- data.frame(
  Test = c("Model 3 vs Model 1", "Model 4 vs Model 1"),
  `F-statistic` = c(round(fstat1, 3), round(fstat2, 3)),
  `p-value` = c(round(pval1, 3), round(pval2, 3))
  )

xtable(f_tests, caption = "F-Tests Comparing Nested Models", label = "tab:ftests")
