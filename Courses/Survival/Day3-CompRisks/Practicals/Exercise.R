
#Practicals day 3 - Exercise 2b

#Libraries
library(survival)
library(timereg)

#Data
sir.adm <- read.csv2("http://publicifsv.sund.ku.dk/~frank/data/sir.adm.csv")
sir.adm$pneu <- factor(sir.adm$pneu, labels=c("No pneumomia","Pneumomia"))

#b1) Estimate the cause-specific cumulative hazards for both unit death and discharge
#    by the Nelson-Aalen estimator. Plot the cumulative hazards for patients
#    with and without pneumomia at admission.

## Death
na1 <- survfit(Surv(time, status==2)~pneu, data=sir.adm)
par(mfrow=c(1,2))
kmplot(na1,fun="cumhaz",main="Death",xlab="Days")

## Discharge
na2 <- survfit(Surv(time, status==1)~pneu, data=sir.adm)
kmplot(na2,fun="cumhaz",main="Discharge",xlab="Days")

#Questions for the plot:

#Does pneumonia have an impact on the mortality rate?
No, not when looking at cause specific hazard for death - same as no pneumomia
But, prob of discharge lower, thus those with pneu stay a lot longer in the unit, thus having larger
prob of dying due to this

#What about discharge? Do patients with pneumonia at admission stay longer at the unit?
#Do you expect to see more or fewer patients with pneumonia die on unit than patients without pneumonia?

