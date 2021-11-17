rm(list=ls())

# {{{ hands-on exercice 1
library(mvtnorm)

a <- c(0.16,1.70) # futility boundaries
b <- c(2.24,1.70) # efficacy boundaries
I <- c(6.81,13.62) # information levels
delta <- log(2)   # expected treatment effect

# variance covariance matrix
sigma <- matrix(NA,2,2)
diag(sigma) <- 1
sigma[1,2] <- sigma[2,1] <- sqrt(I[1]/I[2])
sigma

# type-I error
alpha1 <- pmvnorm(lower=b[1],upper=Inf,mean=0,sigma=1) # false positive at interim analysis
# same # pnorm(b[1], mean = 0, sd = 1, lower.tail = FALSE)
alpha2 <- pmvnorm(lower=c(-Inf,b[2]),
                  upper=c(b[1],Inf),
                  mean=c(0,0),
                  sigma=sigma) # false positive at final analysis
alpha1 + alpha2

# type-II error
power1 <- pmvnorm(lower=b[1],upper=Inf,mean=sqrt(I[1])*delta,sigma=1) # true positive at interim analysis
power2 <- pmvnorm(lower=c(a[1],b[2]), # ATT !!!! here we assume that we stop for futility if data suggest so.
                  upper=c(b[1],Inf),
                  mean=sqrt(I)*delta,
                  sigma=sigma) # false positive at final analysis
power1 + power2
# }}}
