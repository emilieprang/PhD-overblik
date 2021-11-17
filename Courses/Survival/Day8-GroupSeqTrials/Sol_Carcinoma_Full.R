
rm(list=ls())

# {{{ Part 1

#---Q1 ---
If <- (qnorm(1-0.025) + qnorm(1-0.2))^2/0.5^2
If

#---Q2----
ceiling(If/(0.5*0.5)) # number of obs events needed
# 126


#---Q3---
n <- 176 # sample size

rateT0 <- 0.00167 # hazard rate group 0
rateT1 <- rateT0*exp(-0.5) # hazard rate group 0

# To compute the probability of observing an event during the follow-up of a randomly included  subject
NMC <- 500000 # for monte-carlo simulation
delay <- runif(NMC,min=0,max=1735) # time from study start to inclusion into the trial
NMC0 <- ceiling(NMC/2)
T <- rep(NA,NMC)
T[1:NMC0] <- rexp(NMC0,rate=rateT0)
T[(NMC0+1):NMC] <- rexp(NMC-NMC0,rate=rateT1)
Pevent <- mean(T + delay < 1994) # prob of observing the event of a randomly included subject

# expected number of observed event
n*Pevent
# }}}


# {{{ part 2
#Q4---
library(gsDesign)
planned <- gsDesign(k=5,           # K analyses
                    test.type = 4, # non-binding futility boundaries
                    alpha=0.025,    # type-I error
                    beta=0.2,      # 1-power
                    sfu=sfPower,   # spending function (upper boundaries)
                    sfl=sfPower,   # spending function (lower boundaries)
                    sfupar=2,  # rho=2 (upper)
                    sflpar=2)  # rho=2 (lower)
planned
#--
max(planned$n.I) # value of the inflation factor (i.e. Imax=I_K for If=1)
1.133*n # sample size for group-sequential design
1.133*n*mean(T + delay < 1994) # expected number of observed events
# }}}


# {{{ part 3

#---Q5 to Q8 (loop)----

PlannedWithIf <- gsDesign(k=5,           # K analyses
                          test.type = 4, # non-binding futility boundaries
                          alpha=0.025,    # type-I error
                          beta=0.2,      # 1-power
                          n.fix=If,      # If
                          sfu=sfPower,   # spending function (upper boundaries)
                          sfl=sfPower,   # spending function (lower boundaries)
                          sfupar=2,  # rho=2 (upper)
                          sflpar=2)  # rho=2 (lower)
# Note that this reproduces boundaries in Table 25.2 of the book chapter of Jennison & Turnbull 2013


# below we write a loop to make all analyses

res <- matrix(NA,5,5)
colnames(res) <- c("beta.k","I.k","z.k","a.k","b.k")
res[,"I.k"] <- PlannedWithIf$n.I # initialize with planned information


# update information and do recalculation at each analysis
for(k in 1:5){
    print(paste0("Analysis k=",k))
    #-- read available data at analysis k ----
    dk <- read.table(paste0("~/teaching/KU/SurvivalPhD/GrouprSeqDesign/data/Carcinoma",k,".csv"),sep=",",header=TRUE)
    #--- fit cox model based on available data at analysis k-----
    coxk <- coxph(Surv(time,status)~trt+strata(inst)+factor(site)+T+N+cond+factor(sex),data=dk)
    ## summary(coxk)
    ## print(paste0("n=",nrow(dk)))
    ## print(head(dk,n=10))
    #-- save important results of Cox fit----
    Ik <- 1/(summary(coxk)$coef[1,3])^2
    zk <- summary(coxk)$coef[1,4]
    betak <- summary(coxk)$coef[1,1]
    res[k,c("beta.k","I.k","z.k")] <- c(betak,Ik,zk)
    #---- group sequential analysis---
    xk <- gsDesign(k=5,
                   test.type = 4,
                   alpha=0.025,
                   beta=0.2,
                   sfu=sfPower,
                   sfl=sfPower,
                   sfupar=2,
                   sflpar=2,
                   n.fix=If,
                   n.I=res[,"I.k"], # Observed I_1 and I_max
                   maxn.IPlan = res[5,"I.k"]) # I_max
    res[k,"b.k"] <- xk[["upper"]]$bound[k]
    res[k,"a.k"] <- xk[["lower"]]$bound[k]
    if(k==nrow(res)) res[k,"a.k"] <- xk[["upper"]]$bound[k]
    print(res[1:k,])
}
round(res,2)
# }}}

# }}}


