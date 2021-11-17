
#### Day 7 practicals ####

#Libraries
library(ggplot2)
library(data.table)
library(survival)
library(ggfortify)

#### Exercise 1 ####

#Task 1

#Load data
dt <- read.table("C:/Users/epnielsen/OneDrive - Hjerteforeningen/Dokumenter/Courses/Survival/Data/rhdnase-rev.txt",
                 header=T)
dt <- setDT(dt)
dt.sub <- dt[etype==1,]

#Task 2 - estimate intensities non parametrically

#Kaplan meier
surv.obj <- survfit(Surv(start,stop,status==1) ~ trt, data = dt.sub, conf.type="log-log")
autoplot(surv.obj)

#Nelson Aalen
surv.obj1 <- survfit(Surv(start,stop,status==1) ~ trt, data = dt.sub, conf.type="log-log", type=c("fleming-harrington"))
autoplot(surv.obj1, fun="cumhaz")

#Task 3 - fit AG type cox model
m.AG.cox <- coxph(Surv(start,stop,status==1)~trt,data=dt.sub,ties="breslow")

#Task 4 - add covariate meanfev 
m.AG.cox.1 <- coxph(Surv(start,stop,status==1)~trt+meanfev,data=dt.sub,ties="breslow")
#Samme treatment effect, da de er randomiserede og værdien af meanfev derfor ikke burde påvirke behandlingen

#Task 5 - add fct of previous exacerbations
m.prev <- coxph(Surv(start,stop,status==1)~trt+I(enum>1),data=dt.sub,ties='breslow')
m.prev
#The treatment effect becomes smaller. Betinget den del væk der afhænger af tidligere begivenheder.
#Så ikke længere total effekt, kun direkte effect af behandlingen.

#Task 6 - stratify on enum
m.PWP <- coxph(Surv(start,stop,status==1)~trt+strata(enum),data=dt.sub,ties='breslow')

#Task 7 - robust standard error
m.robust <- coxph(Surv(start,stop,status==1)~trt+cluster(id),data=dt.sub,ties='breslow')
#Does not effect the treatment effect, same estimation equations, only variance changes. 

#Task 8
m.frail <- coxph(Surv(start,stop,status==1)~trt+frailty(id),data=dt.sub,ties='breslow')
summary(m.frail)
#Forholdet mellem intensitetet for en given person hvis han var behandlet vs. han var ubehandlet
#Fortolke inden for hvert frailty niveau.

#### Exercise 2 ####

# Task 1
#Kaplan meier
surv.obj.2 <- survfit(Surv(startold,stop,status==1) ~ trt, data = dt.sub, conf.type="log-log")
autoplot(surv.obj.2)

#Nelson Aalen
surv.obj.2.1 <- survfit(Surv(startold,stop,status==1) ~ trt , data = dt.sub, conf.type="log-log", type=c("fleming-harrington"))
autoplot(surv.obj.2.1, fun="cumhaz")

#Task 2 (+cluster(id) giver robuste standard errors)
m.cox <- coxph(Surv(startold,stop,status==1)~trt + cluster(id),
               data=dt.sub,ties="breslow")

#Task 3
m.cox.1 <- coxph(Surv(startold,stop,status==1)~trt+meanfev+cluster(id),
                 data=dt.sub,ties="breslow")

#Task 4
#Vi kan ikke betinge med antal events i en marginal model, da vi står på tid 0
#og forudsiger hvad der sker til tid t, og vi ville derfor betinge med fremtiden
#I modellen for intensiteten, der står vi på tid t og betinger med historien.

#### Exercise 3 ####

#Task 1
attach(bladder1)
dt <- setDT(copy(bladder1))
dt.sub <- dt[treatment!="pyridoxine",]

#Task 2
#Kaplan meier
surv.obj.3 <- survfit(Surv(start,stop,status==1) ~ treatment, data = dt.sub, conf.type="log-log")
autoplot(surv.obj.3)

#Task 3 - fit AG type cox model for intensity 
m.AG.cox <- coxph(Surv(start,stop,status==1) ~ treatment, data = dt.sub, ties="breslow")
m.AG.cox

#Task 4 - cox model for hazard of death
m.AG.cox.death <- coxph(Surv(start,stop,status>1) ~ treatment, data = dt.sub, ties="breslow")
m.AG.cox.death
#When intensity, use competing risk as censoring.

#Task 5 - Estimate non-parametrically the cumulative mean function in the
#two treatment groups using the recmarg function in the mets package

library(mets)
SURV<-phreg(Surv(start,stop,status>1)~strata(treatment),data=dt.sub,km=TRUE)
REC<-phreg(Surv(start,stop,status==1)~strata(treatment),data=dt.sub,km=TRUE)
CMF<-recmarg(REC,SURV)
bplot(CMF) #Her plottes den marginale middelværdi(aalen-johansen agtige estimator)

#Task 6 - Estimate the treatment effect on the mean function in a Ghosh-Lin
#regression model using the recreg function in the mets package

dt.sub$cens<-ifelse(dt.sub$status==0,1,0)
m.goshlin <- recreg(EventCens(start,stop,status,cens)~factor(treatment)+cluster(id),
       data=dt.sub,cause=1,death.code=c(2,3),cens.code=1,cens.model~1)
summary(m.goshlin)






