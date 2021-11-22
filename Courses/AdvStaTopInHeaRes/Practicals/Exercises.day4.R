
#Day 4

library(glmnet)
library(mlbench)
library(bootstrap)
library(boot)
library(selectiveInference)
library(tibble)
library(dplyr)
library(plyr)

install.packages(c("glmnet", "mlbench", "bootstrap", 
                   "boot", "selectiveInference"))

#Data
library(MASS)
data(biopsy)
predictors <- biopsy[complete.cases(biopsy),2:10]
fit <- prcomp(predictors, scale=TRUE)
outcome <- biopsy$class[complete.cases(biopsy)]
DF <- data.frame(outcome, PC1=fit$x[,1], PC2=fit$x[,2], PC3=fit$x[,3], PC4=fit$x[,4])
res <- glm(outcome ~ PC1 + PC2 + PC3 + PC4, data=DF, family=binomial)

#Estimate CV error rate
set.seed(111)
CV_rate <- cv.glm(DF, res) #Default is average squared error function
CV_rate$delta

#Different cost function
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

CV_rate <- cv.glm(DF, res, cost=cost, K=10) #Default is average squared error function
CV_rate$delta


#Penalized regression

lassodata <- load(url("https://www.biostatistics.dk/teaching/advtopicsA/data/lassodata.rda"))

phenotype #Outcome 
genotype #Predictors

#Fit a lasso model
lasso_logreg <- glmnet(genotype, phenotype, family = "binomial")
plot(lasso_logreg, xvar = "lambda", label = TRUE, lwd = 1.5)

#Standardize is default
#If they were on different scales, it would make sense to standardise
#If one is larger for example, we would need to penalise this more, and therefore
#it would seem more important than it actually is.

lasso_logreg_cv <- cv.glmnet(genotype, phenotype, family = "binomial", nfolds = 10)
plot(lasso_logreg_cv)

with(lasso_logreg_cv, data.frame(lambda.min, lambda.1se))

#Extract coefficients for lambda min
lasso_logreg_coefs <- coef(lasso_logreg, s = 0.01368534)
lasso_logreg_coefs[which(lasso_logreg_coefs != 0), 1]

#Ridge regression
ridge_logreg <- update(lasso_logreg, alpha = 0)
plot(ridge_logreg, xvar = "lambda")

#Yes, you can consider when the coefficients are close to zero - same picture as lasso

ridge_logreg_cv <- cv.glmnet(genotype, phenotype, family = "binomial", nfolds = 10, alpha=0)
plot(ridge_logreg_cv)

with(ridge_logreg_cv, data.frame(lambda.min, lambda.1se))

#Extract coefficients for lambda min
ridge_logreg_coefs <- coef(ridge_logreg, s =  0.08204146)
ridge_logreg_coefs[which(ridge_logreg_coefs != 0), 1]

#Elastic net
elastic_logreg <- update(lasso_logreg, alpha = 0.5)
plot(elastic_logreg, xvar = "lambda")










