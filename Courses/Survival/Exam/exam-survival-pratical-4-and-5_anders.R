## ----------------- Set up competing event data: -----------------

## Packages and data
library(frailtypack)
library(data.table)
library(prodlim)
library(riskRegression)
data(colorectal)
setDT(colorectal)

## Function to construct the dataset
setDT(colorectal)
sub.new.les <- function(n.new.les, data=colorectal){
    dd <- copy(colorectal)
    setorder(dd, id, time0)
    ## Mark when n.new.les number of lesions is reached:
    dd[, n.new.reached:=1*(cumsum(new.lesions)>=n.new.les), by = id]
    sub.out <- rbind(
        dd[n.new.reached==0], ## All observations which have not yet the required number of events
        dd[n.new.reached==1, .SD[1], by=id] ## First observation with the required number of events
    )
    setorder(sub.out, id, time0)
    sub.out <- sub.out[, .SD[.N], by=id] ## Extract only last observation
    sub.out[, n.les.status:=0]
    sub.out[n.new.reached==1, n.les.status:=1]
    sub.out[state==1, n.les.status:=2]
    return(sub.out[])
}

## Cause-specific Cox models with competing risk for at least 1 or 2 events:
CSC(Hist(time1, n.les.status)~treatment + age + who.PS , data=sub.new.les(1))
CSC(Hist(time1, n.les.status)~treatment + age + who.PS , data=sub.new.les(2))

## Plot of the marginal probability of at least 1 or 2 events:
plot(prodlim(Hist(time1, n.les.status)~1 , data=sub.new.les(1)))
plot(prodlim(Hist(time1, n.les.status)~1 , data=sub.new.les(2)))
## Think this looks similar to the plots from Scheikes package?




