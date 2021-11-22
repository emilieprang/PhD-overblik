
#Backwards elimination

Epo <- read.csv("http://publicifsv.sund.ku.dk/???helene/Epo.csv", stringsAsFactors=TRUE)

#Full model

full <- lrm(Y~age+sex+HbBase+Treat+Resection+Receptor,data=Epo)
fastbw(full)
bw <- lrm(Y~sex+HbBase+Treat+Receptor,data=Epo)

#Delete age, resection

#Reduced data (subset of 130)
library(rms)
set.seed(911)
Epo17 <- Epo[sample(1:149,replace=FALSE,size=130),]
sub <- lrm(Y~age+sex+HbBase+Treat+Resection+Receptor, data=Epo17)
fastbw(sub)
subbw <- lrm(Y~HbBase+Treat+Receptor, data=Epo17)

#Remove resection, age, sex

#Predict prob for new patient
newpatient <- read.csv("http://publicifsv.sund.ku.dk/???helene/newpatient", stringsAsFactors=TRUE)

library(riskRegression)

pfull=predictRisk(full,newdata=newpatient)
pbw=predictRisk(bw,newdata=newpatient)
psubbw=predictRisk(subbw,newdata=newpatient)

# table results
res=cbind(round(100*c(pfull,pbw,psubbw),1))
rownames(res)=c("Full model","BW all data","BW subset")
colnames(res)=c("Predicted chance (%)")
res

#Random forest exercise

library(randomForestSRC) # load the R library

RFmodel <- rfsrc(Y~age+sex+HbBase+Treat+Resection+Receptor,
        data = Epo, mtry = 2, ntree = 500,
        nodesize = 5)
round(RFmodel$predicted[1:5],3) #Predictions

#Bootstrap samples with replacement
set.seed(911)
n <- nrow(Epo)
bootstrap.sample <- sample(1:n, n, replace = TRUE)
length(unique(bootstrap.sample)) # number of unique patients
length(unique(bootstrap.sample))/n # percentage

#Fit classification trees
tree1 <- rfsrc(Y ~ HbBase, data = Epo, ntree = 1, seed = 911)
tree2 <- rfsrc(Y ~ age, data = Epo, ntree = 1, seed = 911)
tree3 <- rfsrc(Y ~ age+HbBase, data = Epo, ntree = 1, seed = 911)

plot(get.tree(tree1,1))

#Predict chance of successful treatment
predict(tree1, newdata = newpatient, type = "response")$predicted
predict(tree2, newdata = newpatient, type = "response")$predicted
predict(tree3, newdata = newpatient, type = "response")$predicted

#Fit random forests

#Now fit three random forest models each with 500 trees (ntree = 500) using all 6
#predictors (age, HbBase, Treat, Resection, Receptor, sex). Fit two forests with
#the same seed and the last forest with a different seed.
rf1 <- rfsrc(Y ~ HbBase+age+Treat+Resection+Receptor+sex, data = Epo, ntree = 500, seed = 911)
rf2 <- rfsrc(Y ~ HbBase+age+Treat+Resection+Receptor+sex, data = Epo, ntree = 500, seed = 911)
rf3 <- rfsrc(Y ~ HbBase+age+Treat+Resection+Receptor+sex, data = Epo, ntree = 500, seed = 1)

plot(get.tree(rf1,5)) #Plot tree number 5 for example

#Predict the chance of successful treatment for newpatient using all three random
#forest models.
predict(rf1, newdata = newpatient, type = "response")$predicted
predict(rf2, newdata = newpatient, type = "response")$predicted
predict(rf3, newdata = newpatient, type = "response")$predicted

#Are the predictions more stable across the models compared to the classification tree models?
#Yes, more stable predictions.

#Does it the seed influence the predictions of the random forest models with a
#clinically significant magnitude?
#No, around 40% chance of sucessfull treatment for the new patient.



# CV exercise

set.seed(5) # set seed
n <- dim(Epo)[1]
val.set <- sample(x = 1:n, size = n/10, replace=FALSE) # create val.
train.set <- (1:n)[!(1:n) %in% val.set] # create train indicator
Epo.train <- Epo[train.set,]
Epo.val <- Epo[val.set,]

Epo.val$prediction <- 0.5 # set prediction to 50%
loss.fun <- function(Y, Phat) mean((Y - Phat)^2) # define the Brier Score
loss.fun(Epo.val$Y, Epo.val$prediction)

#Fit random forest on the training data
rf.train <- rfsrc(Y ~ HbBase+age+Treat+Resection+Receptor+sex, data = Epo.train, 
                  ntree = 500, seed = 911)

Epo.val$prediction <- predict(rf.train,
                             newdata = Epo.val, type = "reponse")$predicted

loss.fun(Epo.val$Y, Epo.val$prediction) #Brier score
#Lower than the other brier score, so yes, better than the 50% model

#Repeat with another seed
rf.train.1 <- rfsrc(Y ~ HbBase+age+Treat+Resection+Receptor+sex, data = Epo.train, 
                  ntree = 500, seed = 111)

Epo.val$prediction.1 <- predict(rf.train.1,
                              newdata = Epo.val, type = "reponse")$predicted

loss.fun(Epo.val$Y, Epo.val$prediction.1) #Brier score
#Not compare, then you need to have the same validation set.

#Brier score of zero means perfect accuracy
#This means that our predictions are equal to the observed values
#This can happen in theory, however, in the real world we would have probabilities
#and not only zeros and ones. 

#Coin toss - on average give you 50% 
#Coin toss worse than to simply don't know. Worse to guess one or the other than to just
#say 50% (I dont know).
Epo.val$prediction.2 <- rbinom(nrow(Epo.val), 1, p=0.5)
loss.fun(Epo.val$Y, Epo.val$prediction.2) #Brier score

#Assigning 1 to all
Epo.val$prediction.2 <- 1
loss.fun(Epo.val$Y, Epo.val$prediction.2) #Brier score


p.obj <- predict(rf.train.1,newdata = Epo.val, type = "reponse")

p.obj$predicted.oob






