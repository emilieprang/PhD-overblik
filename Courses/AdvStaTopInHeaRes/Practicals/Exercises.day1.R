
#### Packages and libraries ####

install.packages(c("pls", "glmnet", "lme4", "grf",
                   "data.table", "randomForest", "party",
                   "randomForestSRC", "ranger", "mice", 
                   "dataMaid", "naniar", 
                   "Matching", "ggplot2", "sandwich"))

install.packages(c("MESS", "MASS", "pls", "caret", "mlbench"))

library(pls)
library(glmnet)
library(lme4)
library(grf)
library(data.table)
library(randomForest)
library(randomForestSRC)
library(party)
library(ranger)
library(mice)
library(dataMaid)
library(naniar)
library(Matching)
library(ggplot2)
library(sandwich)
library(MESS)
library(MASS)
library(caret)
library(mlbench)
library(bootstrap)


#### Day 1: Multiple testing exercise ####

data(superroot2)

pval <- by(superroot2,
           superroot2$gene,
           FUN=function(dat) {anova(lm(log(signal) ~ array + color + plant, data=dat))[3,5]} )


#1: How many genes would we deem to be significant by chance?

0.05*21500

#4: List 8 smallest p-values

pval.dt <- data.table(pval)
pval.dt.sort <- pval.dt[order(pval)]
head(pval.dt.sort,8)

#We have more significant p-values than what we would expect by chance (0.05*21500)
sum(pval.dt$pval < 0.05, na.rm=T)

#5: Use Holm's correction
head(p.adjust(pval.dt.sort$pval, method="holm"),10)

#6: FDR correction ####
head(p.adjust(pval.dt.sort$pval, method="fdr"),15)

#7: Would you conclude that there are any significantly differentially expressed genes?

#No sign p-values

#8: Does this conclusion change for any of the methods?####

#No, both methods does not give sign p-values, however, a lot smaller with fdr

#9: Do the Bonferroni correction yourself and see how that changes your results.

#Corrected p-value: 0.05/21500 or do this
head(pval.dt.sort$pval*21500)

#Use the p-adjust function:
head(p.adjust(pval.dt.sort$pval, method="bonferroni"),10)
#Not identical, as one model could not fit, so we have only 21499 tests.

#10: Why are the smallest p-values for the FDR correction all identical?

#Identify sets of genes that are of interest
#Because we choose a cut-off and all the values before will get the value as the smallest value before that
#For example cut-off of 11%. No significant results on a 5% level, but choose for example 15% and then we can 
#find a set that will be acceptable (how many values are below 15%).

#11: When might these multiple testing methods be inadequate?####

#When we do not have independence. Then it is too conservative corrections, high price to pay. 

#12: Why might it not always be a good idea to focus on p values (think about prediction and discovery, and the experimental setup)



#### Day 1: Bootstrap exercise ####

data(PimaIndiansDiabetes2)

insulin <- PimaIndiansDiabetes2$insulin[!is.na(PimaIndiansDiabetes2$insulin)] #Remove missing

#1: plot insulin distribution

hist(insulin)

#2: compute 90% quantile

quantile(insulin, p=0.9)

#3: jackknife estimate

jackknife(insulin, median)
est.q <- jackknife(insulin, quantile, p=0.9)

#Normal CI
mean(est.q$jack.values)+c(-1.96,1.96)*est.q$jack.se #CI

#Bias corrected
mean(est.q$jack.values)+est.q$jack.bias+c(-1.96,1.96)*est.q$jack.se #CI bias corrected

#4: bootstrap
boot <- bootstrap(insulin, 1000, quantile, p=0.9)
hist(boot$thetastar)

#CI assuming symmetry
mean(boot$thetastar)+c(-1.96,1.96)*sd(boot$thetastar)

#Confidence interval assuming no symmetry
q.3 <- function(x) quantile(x, p=c(0.025, 0.975))
q.3(boot$thetastar)

#Bias corrected CI with bcanon
bcanon(insulin, 1000, quantile, p=0.9)$confpoints[c(1,8),]

#### PCA exercise: day 1 ####

data(biopsy)
names(biopsy)
predictors <- biopsy[complete.cases(biopsy),2:10]
fit <- prcomp(predictors, scale=TRUE) #Scale in order to make the variables span the same, giving the variables the same weight
summary(fit)  #PC1 explains 66% of the variation

plot(fit) #Gives the variances, the first one is 2.4^2, the next 0.88^2 and so on
biplot(fit) #The numbers are scaled in the plot.

fit$rotation #All variables entering in PC1 with approx same weight, same as average of the variables. 
#Almost only V9 contributes to PC2 (90% of variation).

fit.1 <- prcomp(~., data = predictors, scale=TRUE)

summary(fit.1)

