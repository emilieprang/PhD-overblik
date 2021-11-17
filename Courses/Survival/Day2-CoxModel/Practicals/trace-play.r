
library(mets)
data(TRACE)

system.time(
ca2 <- phreg(Surv(time,status!=0)~vf+chf+diabetes,data=TRACE)
)
system.time(
ccca2 <- 
cox.aalen(Surv(time,status!=0)~prop(vf)+prop(chf)+prop(diabetes),
		 data=TRACE)
)

system.time(
cca2 <- coxph(Surv(time,status!=0)~vf+chf+diabetes,data=TRACE)
)


system.time(
gg <- gof(ca2)
)
par(mfrow=c(1,3))
plot(gg)


sca2 <- phreg(Surv(time,status!=0)~strata(vf)+chf+diabetes,data=TRACE)
gof(sca2)
#
ssca2 <- phreg(Surv(time,status!=0)~strata(vf,chf)+diabetes,data=TRACE)
gof(ssca2)
plot(ssca2)
#


TRACE$dead <- TRACE$status!=0
tt <- survSplit(Surv(time,dead)~.,TRACE,cut=3/12,episode="late")
tt <- transform(tt,vflate=vf*(llate==2),chflate=chf*(llate==2))
head(tt)

ta2 <- coxph(Surv(tstart,time,dead)~vf+chf+diabetes,data=tt)

cta2 <- coxph(Surv(tstart,time,dead)~vf+vflate+chf+chflate+diabetes+cluster(id), data=tt)

## what is this ?? predictions 
pcta2=survfit(cta2,data.frame(vf=c(0,1),chf=0,diabetes=0,vflate=0,chflate=1))
pcta2$surv


par(mfrow=c(1,2))
pa2 <- coxph(Surv(time,status!=0)~vf+chf+diabetes,data=TRACE)
ppa2=survfit(pa2,data.frame(vf=c(0,1),chf=0,diabetes=0))
plot(ppa2,conf.int=TRUE)
###
pa2 <- coxph(Surv(time,status!=0)~strata(vf,chf)+diabetes,data=TRACE)
ppa2=survfit(pa2,data.frame(vf=c(0,1),chf=0,diabetes=0))
plot(ppa2,conf.int=TRUE)

