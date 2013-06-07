# Functions to sample from posterior of symmetric Dirichlet concentration parameter 'alpha'

# Function to calcuate log-likelihood
# K-dim Dirichlet vectors provided in NxK dimensional theta.mat matrix
log.like.alpha <- function(alpha,ltheta.sum,N,K){
  ll <- N*lgamma(alpha*K) - N*K*lgamma(alpha) +
    (alpha-1)*ltheta.sum
  return(ll)
}

# Function to calculate log prior
log.prior.alpha <- function(alpha,nu,tau){
  lp <- (nu-1)*log(alpha) - tau*alpha
  return(lp)
}

# Function to calculate log posterior
# Combines evidence from simplicial vectors with Gamma(nu,tau) prior
log.post.alpha <- function(alpha,ltheta.sum,nu,tau,N,K){
  log.like <- log.like.alpha(alpha=alpha,ltheta.sum=ltheta.sum,N=N,K=K)
  log.prior <- log.prior.alpha(alpha=alpha,nu=nu,tau=tau)
  log.post <- log.like + log.prior
  return(log.post)
}

# Metropolis sampler for alpha
metroh.alpha <- function(alpha.old,theta.mat,nu,tau,prop.var=1,ndraw=1){
  alpha.out <- rep(NA,ndraw)
  ltheta.mat.raw <- log(theta.mat)
  ltheta.mat <- max(ltheta.mat.raw,-700)
  ltheta.sum <- sum(log(theta.mat))
  N <- nrow(theta.mat)
  K <- ncol(theta.mat)
  for(i in 1:ndraw){
    # Get sample from proposal distribution
    alpha.cand <- exp(rnorm(n=1,mean=log(alpha.old),sd=sqrt(prop.var)))
    # Get Metropolis ratio
    lp.old <- log.post.alpha(alpha=alpha.old,ltheta.sum=ltheta.sum,nu=nu,tau=tau,N=N,K=K)
    lp.cand <- log.post.alpha(alpha=alpha.cand,ltheta.sum=ltheta.sum,nu=nu,tau=tau,N=N,K=K)
    r <- lp.cand + log(alpha.cand) - lp.old - log(alpha.old)
    accept <- r > log(runif(1))
    if(accept) {alpha <- alpha.cand} else {alpha <- alpha.old}
    alpha.out[i] <- alpha
    alpha.old <- alpha
  }

  return(alpha.out)
}

