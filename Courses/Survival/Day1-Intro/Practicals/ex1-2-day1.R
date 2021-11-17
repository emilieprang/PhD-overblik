mel<-read.table('e:/advsurv/melanom.txt',header=T) # data file placed on E directory
library(survival)
km<-survfit(Surv(days,dc!=2)~sex,data=mel)
plot(km,conf.int=T) # confidence intervals are added to KM plots
naa<-survfit(Surv(days,dc!=2)~sex,data=mel,type='fleming-harrington')
# the Fleming-Harrington option makes sure that the survival function is estimated as
# exp(-Nelson-Aalen)
plot(naa,fun='cumhaz',conf.int=T) # and when plotted with the fun=cumhaz option we get
# Nelson-Aalen back
survdiff(Surv(days,dc!=2)~sex,data=mel) # gives the logrank test
mel$tykgrp<-1*(mel$thick<2)+2*(2<=mel$thick & mel$thick<5)+3*(mel$thick>=5)
# a new variable, tykgrp, is created with 3 levels of tumor thickness
table(mel$tykgrp)
km2<-survfit(Surv(days,dc!=2)~tykgrp,data=mel)
plot(km2,conf.int=T)
naa2<-survfit(Surv(days,dc!=2)~tykgrp,data=mel,type='fleming-harrington')
plot(naa2,fun='cumhaz')
survdiff(Surv(days,dc!=2)~tykgrp,data=mel)


fyn<-read.table('e:/advsurv/fyn.txt',header=T)
naaf<-survfit(Surv(inage,exage,fail==1)~sex,data=fyn,type='fleming-harrington')
# a survival object with delayed entry is created
plot(naaf,fun='cumhaz')
survdiff(Surv(inage,exage,fail==1)~sex,data=fyn) # NB this does not work with delayed entry
fyn$indur<-fyn$inage-fyn$debage
fyn$exdur<-fyn$exage-fyn$debage
naaf1<-survfit(Surv(indur,exdur,fail==1)~sex,data=fyn,type='fleming-harrington')
# a new survival object with delayed entry in duration scale is created
plot(naaf1,fun='cumhaz')
# the logrank test may be obtained (also with delayed entry) as the score test
# in a Cox model
coxf<-coxph(Surv(inage,exage,fail==1)~sex,data=fyn)
summary(coxf)
coxf1<-coxph(Surv(indur,exdur,fail==1)~sex,data=fyn)
summary(coxf1)

