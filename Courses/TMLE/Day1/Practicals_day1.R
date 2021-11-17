
#### Day one practicals ####

#Libraries
library(tmle)
library(ggplot2)
library(data.table)
library(randomForestSRC)
library(SuperLearner)
library(ltmle)

#Task 1 and 2
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

#Mean 
E_p0_Y0 <- mean(sim(1e6)$df_count$Y0)
E_p0_Y1 <- mean(sim(1e6)$df_count$Y1)
true.ATE <- mean(sim(1e6)$df_count$Y1-sim(1e6)$df_count$Y0)

#Task 3
#Simulate the data
df <- sim(1000)$df_obs

####Outcome model (g-formula)####
fit.f <- glm(Y~A+X1+X2+X3, family=binomial, data=df)

dt <- data.table(df)
#g-formula estimates
dt[, pred.glm.A1:=predict(fit.f, type="response", newdata=
                            copy(dt)[, A:=1])]

dt[, pred.glm.A0:=predict(fit.f, type="response", newdata=
                            copy(dt)[, A:=0])]

#Formula on p. 24
fit.f <- dt[, mean(pred.glm.A1-pred.glm.A0)]
fit.f

####Propensity score####
fit.pi <- glm(A~X1+X2+X3, family=binomial, data=df)
prop_score <- fitted(fit.pi, type="response")

#Use the formula on p. 24
tmp <- (A*Y)/prop_score-((1-A)*Y)/(1-prop_score)
mean_tmp <- mean(tmp)
mean_tmp

#### Task 4 ####

fit.f2 <- glm(Y~A+X1.squared+X2+X3, family=binomial,
              data=dt[, X1.squared:=X1^2])

#g-formula estimates#
dt[, pred.glm.A1:=predict(fit.f2, type="response", newdata=
                            copy(dt)[, A:=1])]

dt[, pred.glm.A0:=predict(fit.f2, type="response", newdata=
                            copy(dt)[, A:=0])]

#Formula on p. 24
fit.f2 <- dt[, mean(pred.glm.A1-pred.glm.A0)]
fit.f2

fit.f
#The estimates are similar but not the same. We added a squared term, thus this model is closer to the true model
#that we simulated from

#Task 5 - fit a random forest model

fit.rf.f <- rfsrc(Y~A+X1+X2+X3, data=dt)

                  
