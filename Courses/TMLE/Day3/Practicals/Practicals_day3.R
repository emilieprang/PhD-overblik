
#### Day three practicals ####

#Libraries
library(tmle)
library(ggplot2)
library(data.table)
library(randomForestSRC)
library(SuperLearner)
library(ltmle)

set.seed(1)
sim.fun <- function(n, intervene=list()) {
  # baseline covariates
  X0.1 <- runif(n, -2, 2)
  X0.2 <- rnorm(n)
  X0.3 <- rbinom(n, 1, 0.2)
  # baseline treatment (randomized)
  if ("A0" %in% names(intervene)) {
    A0 <- intervene$A0
  } else {
    A0 <- rbinom(n, 1, 0.5)
  }
  # follow-up covariates
  X1.1 <- rbinom(n, 1, plogis(-0.7 + 0.3*X0.3 + 0.8*A0))
  X1.2 <- rbinom(n, 1, plogis(0.25 - 0.55*X0.3))
  # follow-up treatment
  if ("A1" %in% names(intervene)) {
    A1 <- intervene$A1(X1.1)
  } else {
    A1 <- rbinom(n, 1, prob=plogis(0.9 - 5*(1-A0) - 4.7*X1.1 - 4.8*X1.2))
  }
  # outcome
  Y <- rbinom(n, 1, prob=plogis(-0.9 - 0.2*A0 + 1.2*X1.1 - 0.1*A1 - 0.8*A1*(X1.1==0)))
  if (length(names(intervene))>0) {
    return(mean(Y))
  } else {
    return(data.table(X0.1=X0.1, X0.2=X0.2, X0.3=X0.3,
                      A0=A0,
                      X1.1=X1.1, X1.2=X1.2,
                      A1=A1,
                      Y=Y))
  }
}

#Task 1

dt <- sim.fun(2000)

#Task 2

fit.A0 <- glm(A0 ~ X0.1 + X0.2 + X0.3, family = "binomial", data=dt)

fit.A1 <- glm(A1 ~ X0.1 + X0.2 + X0.3 + X1.1 + X1.2 + A0, family="binomial", data=dt)


dt[,H.1:=(A0==1)/predict(fit.A0, type="response")]
dt[,H.2:=((A1==1)*H.1)/predict(fit.A1, type="response")]

#Task 3

fit.f <- glm(Y ~ X0.1 + X0.2 + X0.3 + X1.1 + X1.2 + A0 + A1, family="binomial", data=dt)
dt.new <- copy(dt)
dt.new[,A1:=0]
Q2 <- predict(fit.f, type="response", data=dt.new)

#Task 4
fit.log <- glm(Y~offset(plogis(Q2)), weights = H.2, family = "binomial", data=dt)
Q2.star <- predict(fit.log, type="response")

mean(dt$H.2*(dt$Y-Q2.star))

#Task 5
dt[,Q2.star:=Q2.star]
fit.Q2 <- glm(Q2.star ~ X0.1 + X0.2 + X0.3 + A0, family=quasibinomial(), data=dt) #Could have used superlearner or other regression here

dt.new <- dt[,A0:=1]
Q1 <- predict(fit.Q2, type="response", data=dt.new)

#Task 6
fit.log <- glm(Q2.star~offset(plogis(Q1)), weights = H.1, family = "quasibinomial", data=dt)
Q1.star <- predict(fit.log, type="response")

dt[,Q1.star:=Q1.star]
mean(dt$H.1*(dt$Q2.star-dt$Q1.star))

#Task 7
tmle.hat <- mean(Q1.star)
tmle.hat

#Task 8 - calculating standard error
(se.tmle.hat <- dt[, sqrt(mean((H.2*(Y-Q2.star) +
                                        H.1*(Q2.star - Q1.star) +
                                        Q1.star - tmle.hat)^2)/nrow(dt))])







                  
