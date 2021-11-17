
#### Super learner practicals ####

#Libraries
library(riskRegression)
library(nnls)
library(foreach)
library(SuperLearner)
library(ranger)
library(randomForest)
library(randomForestSRC)
library(rms)
library(data.table)

#### Task 2 ####

#Load data
dt <- readRDS("C:/Users/epnielsen/OneDrive - Hjerteforeningen/Skrivebord/TMLEcourse/Day2/Practicals/pph.rds")

#Split data
pph09 <- dt[Year==2009]
pph <- dt[!Year==2009]

#Prob of planned ces. section by year
pph[,prob.plannedCS:=sum(plannedCS)/.N, by=Year]
plot(pph$Year, pph$prob.plannedCS)

#Prob of the other variables and median age
pph[,prob.MotherAge:=median(MotherAge), by=Year]

tmp <- pph[,{X <- lapply(names(.SD),function(x)mean(.SD[[x]]=="Yes"))
names(X) <- names(.SD)
data.frame(X)
},.SDcols=c("PrevPPHbin","PrevArgumented","PrevPraeecl","PrevAbruptio","PrevCS","PrevRetained","PrevInduced"),keyby=Year]

plot(tmp$Year, tmp$PrevPPHbin)
plot(tmp$Year, tmp$PrevArgumented)

####Task 3####

#Model fits
fit1 <- glm(plannedCS~MotherAge+PrevArgumented+PrevPraeecl+
              PrevAbruptio+PrevCS+PrevRetained+PrevPPHbin+PrevInduced,data=pph,
            family="binomial")
fit2 <- gam(plannedCS~s(MotherAge,3)+PrevArgumented+PrevPraeecl+
              PrevAbruptio+PrevCS+PrevRetained+PrevPPHbin+PrevInduced,data=pph,
            family="binomial")
pph$Y=factor(pph$plannedCS)
fit3 <- rfsrc(Y~MotherAge + PrevArgumented+PrevPraeecl+
                PrevAbruptio+ PrevCS+PrevRetained+ PrevPPHbin+PrevInduced,data=
                pph,ntree=200,seed=7)

# predict outcome probabilities in data from 2009
pph09[,p1:=predictRisk(fit1,newdata=pph09)]
pph09[,p2:=predictRisk(fit2,newdata=pph09)]
pph09[,p3:=predictRisk(fit3,newdata=pph09)]

#Scatter plot







