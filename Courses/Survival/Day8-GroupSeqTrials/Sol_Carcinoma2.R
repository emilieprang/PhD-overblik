
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
