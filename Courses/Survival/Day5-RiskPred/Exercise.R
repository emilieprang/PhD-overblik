library(riskRegression)
library(randomForestSRC)
library(survival)
library(data.table)
library(prodlim)
library(pec)
library(Rcpp)

####Exercise 1####

#Task 1 - Fit a cox model
data("Melanoma")

m.cox.ph <- CSC(Hist(time, status) ~ age + sex + ulcer + thick + ici + epicel + invasion, data=Melanoma)

# Task 2 - Stratified cox model
m.cox.strat <- CSC(Hist(time, status) ~ age + strata(sex) + ulcer + thick + ici + epicel + invasion,data=Melanoma)

#Samme coef for alle grupperne, men forskellige baseline hazard for de to grupper (mænd/kvinder)

# Task 3 - Random forest

m.rf <- rfsrc(Surv(time, status) ~ age + sex + ulcer + thick + epicel + ici + invasion, data =  Melanoma)

# Task 4 - predict from the models
data.new <- data.table(age=73, epicel=factor("not present"), ulcer=factor("not present"), 
                       thick=3.41, sex=factor("Male"), ici=factor("2"), invasion=factor("level.1"))

pred.rf <- predictRisk(m.rf, data.new, times=365.25*3, cause=1)

pred.cox <- predictEventProb(m.cox.ph, data.new, times=365.25*3, cause=1)

pred.strat.cox <- predictEventProb(m.cox.strat, data.new, times=365.25*3, cause=1)

pred.rf
pred.cox
pred.strat.cox

#### Exercise 2####

#Task 1 - interpretation of AUC
#The probability that the risk of dying after 3 years for person 1 is greater than 
#the risk of dying after 3 years for person 2, given that person 1 died and person 2 survived

#Task 2 - AUC with competing risks

#AUC_C1(t)=P(... | D_new1=1 and (T_new1 < Tnew2 or Dnew2=2))
#So subject one experienced the event (D=1) and subject two experienced event later or not at all

#### Exercise 3####

#Task 1 - Use the score function to plot the brier score and AUC for the 3 models (WRONG, not using CV)

s.obj <- Score(list(cox.ph=m.cox.ph, cox.strat=m.cox.strat, rf=m.rf), 
               formula=Hist(time, status)~1, data=Melanoma, times=seq(0,3500, 100))

summary(s.obj, what="score", times=1500)

plotBrier(s.obj) #Does not work in the R-studio graphic device
plotAUC(s.obj)

# Task 2 - using CV

s.obj <- Score(list(cox.ph=m.cox.ph, cox.strat=m.cox.strat, rf=m.rf), split.method = "CV10",
               formula=Hist(time, status)~1, data=Melanoma, times=seq(0,3500, 100))

summary(s.obj, what="score", times=1500)

BS <- s.obj$Brier$score$Brier
AUC <- s.obj$AUC$score$AUC

#Plot this against time.














