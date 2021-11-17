
#### Day one practicals ####

#Libraries
library(tmle)
library(ggplot2)
library(data.table)
library(randomForestSRC)
library(SuperLearner)
library(ltmle)

#Simulate data
#Libraries
library(tmle)
library(ggplot2)
library(data.table)
library(randomForestSRC)
library(SuperLearner)
library(ltmle)

#Simulate data
sim <- function(n){
  set.seed(15)
  
  X1 <-runif(n, -2, 2)
  X2 <- rnorm(n, mean=0, sd=1)
  X3 <- rbinom(n, 1, prob=0.2)
  
  A <- rbinom(n, 1, prob=plogis(-0.25 + 0.8*X1+0.25*X3))
  Y <- rbinom(n, 1, prob=plogis(-0.9 + 1.9*X1^2+0.6*X2+0.5*A))
  
  df_obs <- data.frame(X1, X2, X3, A, Y)
  
  Y1 <- rbinom(n, 1, prob=plogis(-0.9 + 1.9*X1^2+0.6*X2+0.5*1))
  Y0 <- rbinom(n, 1, prob=plogis(-0.9 + 1.9*X1^2+0.6*X2+0.5*0))
  df_count <- data.frame(X1, X2, X3, Y0, Y1)
  
  return(list(df_obs=df_obs, df_count=df_count))
}

#### Task 1 ####
df <- sim(1000)$df_obs
dt <- data.table(df)

#1.
fit.f <- glm(Y~A+X1+X2+X3, family=binomial, data=dt) #Outcome regression f
fit.pi <- glm(A~X1+X2+X3, family=binomial, data=dt) #Propensit score pi


#2.
dt[, pred.glm.A1:=predict(fit.f, type="response", newdata=
                            copy(dt)[, A:=1])]

dt[, pred.glm.A0:=predict(fit.f, type="response", newdata=
                            copy(dt)[, A:=0])]

#Formula on p. 24
dt[, pred.glm.exp:=mean(pred.glm.A1-pred.glm.A0)]

#3.
dt[, glm.prop.score:=predict(fit.pi, type="response")]

#4. - calculate the clever covariate
dt[, clever.covariate.A1:=((A==1)/glm.prop.score)]
dt[, clever.covariate.A0:=((A==0)/(1-glm.prop.score))]
dt[, clever.covariate:=clever.covariate.A0+clever.covariate.A1]

#5. - run a logistic model

eps.A1 <- glm(Y ~ offset(qlogis(pred.glm.exp))+clever.covariate.A1-1,
                data=dt, family=binomial())

eps.A0 <- glm(Y ~ offset(qlogis(pred.glm.exp))+clever.covariate.A0-1,
                   data=dt, family=binomial())

#6. - predict from the model in 5. (tmle update)

dt[, f.A1.tmle:=predict(eps.A1)]
dt[, f.A0.tmle:=predict(eps.A0)]

dt[,]







                  
