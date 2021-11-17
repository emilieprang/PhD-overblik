

#Exercise 4
#Libraries
library(ggplot2)
library(data.table)
library(survival)
library(ggfortify)
library(timereg)
library(mets)

#Task 1
help(TRACE)
data(TRACE)

dt <- TRACE

#Fit cox model
res.cox <- coxph(Surv(time, status!=0) ~ vf + diabetes + chf, data = dt)
res.cox
summary(res.cox)

#Model validation
test.ph <- cox.zph(res.cox, transform="log")
test.ph

out1 <- phreg(Surv(time, status!=0) ~ vf + diabetes + chf, data = dt)

#Use Survsplit to cut the variables into different time periods.
#Use phreg plot to get an idea where to cut

#Could also use survfit to predict, setting variables equal to values. 
predict(res.cox, type = "survival")

#Robust standard errors can be found using cluster=id
res.cox1 <- coxph(Surv(time, status!=0) ~ vf + diabetes + chf, data = dt, cluster=id)
summary(res.cox1)

