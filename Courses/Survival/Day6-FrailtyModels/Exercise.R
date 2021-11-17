
####Exercise day 6####

library(timereg)
data(diabetes)

#a) Apply standard cox model to the data

m.cox <- coxph(Surv(time, status) ~ treat + adult, data = diabetes)
summary(m.cox)

#b) Clustered cox
m.cox.cluster <- coxph(Surv(time, status) ~ treat + adult + cluster(id), data = diabetes)
summary(m.cox.cluster)

#c) Strafitied cox
m.cox.strat <- coxph(Surv(time, status) ~ treat + adult + strata(id), data = diabetes)
summary(m.cox.strat)
#Interpretation: Comparing within strata, so 0.36 is hazard ratio when comparing treated vs 
#non treated for the same person.. weird. 
#For every individual (both eyes), the adult variable is the same, so we cannot estimate an "adult" effect
#as the adult variable is the same for each person.

#d) Conditional proportional hazards shared gamma frailty model
m.gamma <-  coxph(Surv(time, status) ~ treat + adult + frailty(id), data = diabetes)
summary(m.gamma)
#Interpretation: Interpret as hazard ratios within clusters, so the treat hazard ratio is the hazard ratio
#of treatment within clusters. 
#The adult variable has an odd interpretation (agin same within clusters).

#Alternative method
library(frailtySurv)
m.gamma.1 <- fitfrail(Surv(time, status) ~ treat + adult + cluster(id), frailty="gamma", dat = diabetes)
summary(m.gamma.1)
vcov(m.gamma.1)

#e) Conditional proportional hazards shared log-normal frailty model
library(frailtySurv)
m.lognormal <- fitfrail(Surv(time, status) ~ treat + adult + cluster(id), frailty="lognormal", dat = diabetes)
summary(m.lognormal)
vcov(m.lognormal)

#Alternative method
library(coxme)
summary(coxme(Surv(time, status) ~ treat + adult + (1|id), data = diabetes))



