
#### Practicals day 8
library(gsDesign)
library(data.table)
library(survival)

#Task 4
gsDesign(k=5, # K=2 analyses
         test.type = 4, # non-binding futility boundaries
         alpha=0.025, # type-I error
         beta=0.2, # 1-power
         sfu=sfPower, # spending function (upper boundaries)
         sfl=sfPower, # spending function (lower boundaries)
         sfupar=2, # rho=2 (upper)
         sflpar=2,
         n.fix=177) # rho=2 (lower)

#Or without n.fix=177, we could calculate 177*1,13=201 subjects. 

#Load data
carcinoma1 <- read.table("C:/Users/epnielsen/OneDrive - Hjerteforeningen/Dokumenter/Courses/Survival/Data/Carcinoma1.csv",
                         sep=",", header=T)

dt1 <- setDT(carcinoma1)

carcinoma2 <- read.table("C:/Users/epnielsen/OneDrive - Hjerteforeningen/Dokumenter/Courses/Survival/Data/Carcinoma2.csv",
                         sep=",", header=T)

dt2 <- setDT(carcinoma2)

#Task 5
res.cox <- coxph(Surv(time, status) ~ strata(inst) + factor(sex) + cond + T + N + factor(site) + trt, data = dt1)
summary(res.cox)

#Tag se(coef) for treatment for at udregne informationen
n.fix <- (qnorm(1-0.025)+qnorm(1-0.2))^2 / (0.5^2)
I.1 <- 1/(0.4936^2)

#Dette bruges til at finde forventede observationer i de forskellige analyser
I.exp <- gsDesign(k=5, # K=2 analyses
         test.type = 4, # non-binding futility boundaries
         alpha=0.05, # type-I error
         beta=0.2, # 1-power
         n.fix=n.fix, # If
         sfu=sfPower, # spending function (upper boundaries)
         sfl=sfPower, # spending function (lower boundaries)
         sfupar=2, # rho=2 (upper)
         sflpar=2)$n.I # rho=2 (lower)

#Now we need to calculate the boundaries, if the observed z value (from the trt variable in the cox summary)
#is in between the two values, we continue. So, we continue (-1.89 < -1.598 < 3.21). 
#If it's below, then we stop because the treatment is reallt bad, if it's above the upper limit, then 
#we can stop because the treatment works really well. 

gsDesign(k=5,
         test.type = 4,
         alpha=0.05,
         beta=0.2,
         sfu=sfPower,
         sfl=sfPower,
         sfupar=2,
         sflpar=2,
         n.fix=n.fix, # If
         n.I=c(I.1,I.exp[-1]), # Observed I_1 and I_max
         maxn.IPlan = 35.769308)

summary(res.cox)








