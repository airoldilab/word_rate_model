src.dir <- "../R/"
source(paste0(src.dir,"hparam_sample.R"))
library("MCMCpack")

# Test alpha sampler
D <- 10000
K <- 10
nu <- 1
tau <- 20
alpha <- rgamma(n=1,shape=nu,rate=tau)
print(alpha)
theta.mat <- rdirichlet(n=D,alpha=alpha*rep(1,K))

debug(metroh.alpha)
metro.out <- metroh.alpha(alpha.old=0.01,theta.mat=theta.mat,
                          nu=nu,tau=tau,prop.var=0.05,ndraw=10000)
plot(metro.out,type="b")
hist(metro.out)
