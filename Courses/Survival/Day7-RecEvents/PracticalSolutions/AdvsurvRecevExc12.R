library(survival)
cystfib<-read.table('e:/SACT17/rhdnase-rev.txt',header=T)

#Exc 1 Intensity models

plot(survfit(Surv(start,stop,status==1)~trt,data=subset(cystfib,etype==1)),
     fun='cumhaz')
summary(coxph(Surv(start,stop,status==1)~trt,data=subset(cystfib,etype==1),
      ties="breslow"))
summary(coxph(Surv(start,stop,status==1)~trt+meanfev,data=subset(cystfib,etype==1),
        ties="breslow"))
summary(coxph(Surv(start,stop,status==1)~trt+I(enum>1),data=subset(cystfib,etype==1),
        ties="breslow"))
summary(coxph(Surv(start,stop,status==1)~trt+strata(enum1),data=subset(cystfib,etype==1),
        ties="breslow"))
summary(coxph(Surv(start,stop,status==1)~trt+cluster(id),data=subset(cystfib,etype==1),
        ties="breslow"))
summary(coxph(Surv(start,stop,status==1)~trt+frailty(id),data=subset(cystfib,etype==1),
        ties="breslow"))

#Exc 2 Marginal models

plot(survfit(Surv(startold,stop,status==1)~trt,data=subset(cystfib,etype==1)),
      fun='cumhaz')
summary(coxph(Surv(startold,stop,status==1)~trt+cluster(id),data=subset(cystfib,etype==1),
        ties="breslow"))
summary(coxph(Surv(startold,stop,status==1)~trt+meanfev+cluster(id),
        data=subset(cystfib,etype==1),ties="breslow"))


