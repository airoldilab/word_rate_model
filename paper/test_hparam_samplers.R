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

#debug(metroh.alpha)
metro.out <- metroh.alpha(alpha.old=alpha,theta.mat=theta.mat,
                          nu=NULL,tau=NULL,prop.var=0.005,ndraw=100000)
plot(metro.out,type="b")
hist(metro.out)


# Comparison of two types of Dirichlet model
# Doesn't seem to be much difference
theta2.mat <- rdirichlet(n=K,alpha=alpha*rep(1,D))
metro.out2 <- metroh.alpha(alpha.old=alpha,theta.mat=theta2.mat,
                          nu=NULL,tau=NULL,prop.var=0.005,ndraw=100000)
plot(metro.out2,type="b")
print(c(sd(metro.out),sd(metro.out2)))
par(mfrow=c(1,2))
hist(metro.out)
hist(metro.out2)



# Test psi sampler
V <- 1000
nu <- 1
tau <- 50
kappa <- K*0.05
psi <- rgamma(n=1,shape=nu,rate=tau)
sigma.vec <- rgamma(n=V,shape=kappa,rate=psi)

# Get posterior draws of psi
psi.draws <- draw.psi(sigma.vec=sigma.vec,kappa=kappa,nu=nu,
                      tau=tau,ndraw=10000)
hist(psi.draws)
