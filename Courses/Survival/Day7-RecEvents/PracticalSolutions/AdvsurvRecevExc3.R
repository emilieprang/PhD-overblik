library(survival)
attach(bladder1)
newbladder<-subset(bladder1,treatment != "pyridoxine")
plot(survfit(Surv(start,stop,status==1)~treatment,data=newbladder),fun='cumhaz')
summary(coxph(Surv(start,stop,status==1)~factor(treatment),data=newbladder,
        ties="breslow"))
summary(coxph(Surv(start,stop,status>1)~factor(treatment),data=newbladder,
        ties="breslow"))

library(mets)

survobj<-phreg(Surv(start,stop,status>1)~strata(treatment),data=newbladder,
         km=TRUE)
recevobj<-phreg(Surv(start,stop,status==1)~strata(treatment),data=newbladder,
         km=TRUE)

CMFobj<-recmarg(recevobj,survobj)
bplot(CMFobj)

newbladder$cens<-ifelse(newbladder$status==0,1,0)

GLobj<-recreg(EventCens(start, stop, status, cens)~factor(treatment)+cluster(id),
data=newbladder, cause=1,death.code=c(2,3),cens.code=1,cens.model~1)
summary(GLobj)