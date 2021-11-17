
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

