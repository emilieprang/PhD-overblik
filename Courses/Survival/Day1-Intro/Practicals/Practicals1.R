
#### Day one practicals ####

#Libraries
library(ggplot2)
library(data.table)
library(survival)
library(ggfortify)

#Task 1

#Load data
dt <- read.table("C:/Users/epnielsen/OneDrive - Hjerteforeningen/Dokumenter/Courses/Survival/Data/melanom.txt",
                 header=T)
dt <- setDT(dt)

#Task 2

#All cause mortality
dt[,status:=0]
dt[dc==1 | dc==3, status:=1]
str(dt)

#Survival object
surv.obj <- survfit(Surv(days, status) ~ sex, data = dt, conf.type="log-log")

#Kaplan Meier
plot(surv.obj, conf.int = T, col=c("red", "blue"))
legend("topright", legend=c("Female", "Male"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
autoplot(surv.obj)

#Nelson Aalen
surv.obj1 <- survfit(Surv(days, status) ~ sex, type=c("fleming-harrington"), data = dt, conf.type="log-log")
plot(surv.obj1, conf.int = T, col=c("red", "blue"), fun="cumhaz")

#Task 3

#Log rank test 
surv_diff <- survdiff(Surv(days, status) ~ sex, data = dt)
surv_diff #p value of 0.005, indicating that the sec groups differ sign. on survival

#Task 4
dt[,thick1:=cut(thick,
                        breaks=c(0,2,5,Inf),
                        include.lowest=TRUE,
                        labels=c("<2","[2,5)",">=5"))]
table(dt$thick1)

#Survival object
surv.obj <- survfit(Surv(days, status) ~ thick1, data = dt, conf.type="log-log")

#Kaplan Meier
plot(surv.obj, conf.int = T, col=c("red", "blue", "green"))
legend("topright", legend=c("<2", "[2,5)", ">=5"),
       col=c("red", "blue", "green"), lty=1:2, cex=0.8)
autoplot(surv.obj)

#Nelson Aalen
surv.obj1 <- survfit(Surv(days, status) ~ thick1, type=c("fleming-harrington"), data = dt, conf.type="log-log")

plot(surv.obj1, conf.int = T, col=c("red", "blue", "green"), fun="cumhaz")
legend("topright", legend=c("<2", "[2,5)", ">=5"),
       col=c("red", "blue", "green"), lty=1:2, cex=0.8)
autoplot(surv.obj1)

#Log rank test 
surv_diff <- survdiff(Surv(days, status) ~ thick1, data = dt)
surv_diff #p value of 0.005, indicating that the sec groups differ sign. on survival


#### Fyn data set ####

#Load data
dt <- read.table("C:/Users/epnielsen/OneDrive - Hjerteforeningen/Dokumenter/Courses/Survival/Data/fyn.txt",
                 header=T)
dt <- setDT(dt)

#Age specific hazards
surv.obj <- survfit(Surv(inage, exage, fail==1) ~ sex, type=c("fleming-harrington"), data = dt)
plot(surv.obj, fun="cumhaz")
survdiff(Surv(inage, exage, fail==1) ~ sex, data = dt) #Does not work with right censored data
#Instead, use coxph:
coxf <- coxph(Surv(inage, exage, fail==1) ~ sex, data = dt)
summary(coxf)



