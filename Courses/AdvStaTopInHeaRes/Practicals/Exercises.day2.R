
#### Exercises day 2: missing data ####

load("C:/Users/epnielsen/OneDrive - Hjerteforeningen/Dokumenter/Courses/AdvStaTopInHeaRes/data/alcodata1.rda")

setDT(alcodata1)

summary(alcodata1)

alcodata1$completecase <- "Non-complete" 
alcodata1$completecase[complete.cases(alcodata1)] <- "Complete"

library(ggplot2)
qplot(gender, facets = ~ completecase, data = alcodata1)
qplot(ageBeyond60, facets = ~ completecase, data = alcodata1)
qplot(ageBeyond60, facets = ~ completecase, data = alcodata1)


table(alcodata1$ageBeyond60+60)

library(naniar)

gg_miss_var(alcodata1, facet=gender, show_pct=T)
qplot(dependence, facets = ~ drinks, data = alcodata1)
gg_miss_var(alcodata1, facet = country, show_pct = TRUE)
vis_miss(alcodata1)
gg_miss_upset(alcodata1)
gg_miss_case(alcodata1)
gg_miss_fct(alcodata1, gender)
table(alcodata1$dependence)

# Exercise: Analyze data with missing information 

load("C:/Users/epnielsen/OneDrive - Hjerteforeningen/Dokumenter/Courses/AdvStaTopInHeaRes/data/alcodata1.rda")
setDT(alcodata1)

#Initial approach
alco_ccmodel.1 <- lm(drinks~., data=alcodata1)

#Complete cases:
nrow(na.omit(alcodata1)) #343 complete cases in the dataset
alco.complete <- alcodata1[complete.cases(alcodata1)==T,]

#Fit a linear model ignoring the missing cases
alco_ccmodel <- lm(drinks~., data=alco.complete)
summary(alco_ccmodel)

#Plot the estimates
source("C:/Users/epnielsen/OneDrive - Hjerteforeningen/Dokumenter/Courses/AdvStaTopInHeaRes/R/functions.R")

plotEstimates(`Complete cases`  = alco_ccmodel)

#Use the MICE package

alco_imp <- mice(alcodata1)
View(alco_imp$imp$dependence)
alco_fit <- with(alco_imp, 
                 lm(drinks ~ ageBeyond60 + prevTreat + country + gender + education + partner + dependence))
alco_micemodel <- pool(alco_fit)

summary(alco_micemodel, conf.int = TRUE)

plotEstimates(`Complete cases` =  alco_ccmodel, `MICE` = alco_micemodel)



