
wrm.fit <- function(
             ## Vector of obs word counts
             wc.obs,
             ## Vector of doc ids for word counts
             d.index,
             ## Vector of word ids for word counts
             f.index,
             ## Labeled vector of document lengths
             ## Length 'D' and 
             doc.length.vec,
             ## Number of topics
             ntopics,
             ## Hyperparameters
             beta=1,alpha=0.05,psi=1,
             ## Priors for hyperparameters
             nu.alpha=NULL,tau.alpha=NULL,
             nu.beta=NULL,tau.beta=NULL,
             nu.psi=NULL,tau.psi=NULL,
             ## Should hyperparameters be inferred?
             hparam.draw=TRUE,
             ## Should Metropolis corrections be applied?
             metro.correct=TRUE,
             ## Iterations
             iter=100,burnin=0,
             ## Start sampler from old run?
             ## Will override any hyperparameter preferences
             wrm.old=NULL,
             ## Ndraws for theta sampler
             ndraw.theta=5,
             ## Parameter tracing
             nwords.trace=NULL,ndocs.trace=NULL,
             ## Save output every 100 iters?
             file.out=NULL,
             ## Save burnin draws?
             save.burnin=FALSE,
             verbose=FALSE){

  ## Model inputs
  doc.ids <- unique(d.index)
  word.ids <- unique(f.index)
  D <- length(doc.ids)
  V <- length(word.ids)
  Q <- length(d.index)
  d.index <- factor(d.index)
  f.index <- factor(f.index)
  ndraws <- iter + burnin

  ## Is position from old run provided?
  old.run <- !is.null(wrm.old)
  
  ## Initialize parameters from old run or use uniform start for thetas
  if(old.run){
    theta.mat <- wrm.old$ave.param.list$theta.mat
    lambda.mat <- wrm.old$ave.param.list$lambda.mat
    alpha <- wrm.old$ave.param.list$alpha
    beta <- wrm.old$ave.param.list$beta
    psi <- wrm.old$ave.param.list$psi
  } else {
    theta.mat <- matrix(1/ntopics,ncol=ntopics,nrow=D)
  }

  ## Initalize augmented word count array either from old run or
  ## from uniform Multinomial
  if(old.run){
    wc.aug <- draw.wc.aug(lambda.mat=lambda.mat,theta.mat=theta.mat,Q=Q,
                          wc.obs=wc.obs,d.index=d.index,f.index=f.index)
  } else {
    wc.aug <- multi.init.draw(obs.count=wc.obs,ntopics=ntopics)
  }
  
  ## Get linear operator for topic-specific counts
  A.d <- t(sparse.model.matrix(~ factor(d.index) + 0))
  rownames(A.d) <- levels(d.index)
  A.f <- t(sparse.model.matrix(~ factor(f.index) + 0))
  rownames(A.f) <- levels(f.index)

  for(i in 1:ndraws){

    if(verbose){
      print(i)
      ## Start time
      t0 <-  proc.time()[3]
    }

    ## Is this a burnin draw?
    is.burnin <- i <= burnin
    ## Is this an early burnin draw?
    ## Idea: don't bother with Metropolis corrections for
    ## early part of burnin so that mixing speed is optimized
    is.early.burnin <- i < 0.75*burnin
    apply.mcorrect <- all(!is.early.burnin,metro.correct)

    ## Get new wfk VxK matrix
    wfk.mat <- A.f%*%wc.aug
    ## Get new wdk DxK matrix
    wdk.mat <- A.d%*%wc.aug
    
    ## Get VxK matrix of rates from wfk.mat
    lambda.mat <- lambda.mat.draw(wfk.mat=wfk.mat,psi=psi,
                                  doc.length.vec=doc.length.vec,
                                  theta.mat=theta.mat,beta=beta,
                                  ntopics=ntopics)

    ## Get mu matrix as function of the lambdas
    mu.mat <- log(lambda.mat)
    
    ## Get VxK matrix of phis as a function of the lambdas
    phi.mat <- get.phi.mat(lambda.mat)

    ## Get sigma vector as function of the lambdas
    sigma.vec <- apply(lambda.mat,1,sum)
    
    ## Get DxK matrix of memberships from wdk.mat
    theta.mat <- theta.mat.draw(wdk.mat=wdk.mat,lambda.mat=lambda.mat,
                                theta.mat=theta.mat,alpha=alpha,
                                ntopics=ntopics,
                                doc.length.vec=doc.length.vec,
                                ndraw=ndraw.theta,
                                metro.correct=apply.mcorrect)

    ## Resample hyperparameters if requested
    if(hparam.draw){
      alpha <- metroh.alpha(alpha.old=alpha,theta.mat=theta.mat,
                            nu=nu.alpha,tau=tau.alpha,
                            prop.var=0.005,ndraw=250,
                            last.draw=TRUE,metro.correct=apply.mcorrect)
      beta <- metroh.alpha(alpha.old=beta,theta.mat=phi.mat,
                           nu=nu.beta,tau=tau.beta,
                           prop.var=0.005,ndraw=250,
                           last.draw=TRUE,metro.correct=apply.mcorrect)
      psi <- draw.psi(sigma.vec=sigma.vec,kappa=ntopics*beta,
                      nu=nu.psi,tau=tau.psi,ndraw=1)
    }

    ## Redraw augmented word counts as a funtion to theta and lambda matrices
    ## as well as observed counts
    wc.aug <- draw.wc.aug(lambda.mat=lambda.mat,theta.mat=theta.mat,Q=Q,
                          wc.obs=wc.obs,d.index=d.index,f.index=f.index)

    ## Update posterior mean
    if(!is.burnin){
      pos <- i-burnin
      if(pos==1){
        final.param.list <- setup.final.param.list(mu.mat=mu.mat,theta.mat=theta.mat,
                                                   phi.mat=phi.mat,doc.ids=doc.ids,word.ids=word.ids,
                                                   hparam.draw=hparam.draw,alpha=alpha,beta=beta,psi=psi,
                                                   ndraws.store=iter,D=D,V=V,ntopics=ntopics,
                                                   nwords.trace=nwords.trace,ndocs.trace=ndocs.trace)
        final.param.list <- update.final.param.list(pos=1,mu.mat=mu.mat,theta.mat=theta.mat,
                                                    phi.mat=phi.mat,final.param.list=final.param.list,
                                                    hparam.draw=hparam.draw,alpha=alpha,beta=beta,psi=psi)
        ave.param.list <- list(mu.mat=mu.mat,theta.mat=theta.mat,phi.mat=phi.mat,sigma.vec=sigma.vec)
        if(hparam.draw){
          ave.param.list$alpha <- alpha
          ave.param.list$beta <- beta
          ave.param.list$psi <- psi
        }
      } else {
        final.param.list <- update.final.param.list(pos=pos,mu.mat=mu.mat,theta.mat=theta.mat,
                                                    phi.mat=phi.mat,final.param.list=final.param.list,
                                                    hparam.draw=hparam.draw,alpha=alpha,beta=beta,psi=psi)
        ave.param.list <- update.ave.params(pos=pos,ave.param.list=ave.param.list,
                                            mu.mat=mu.mat,theta.mat=theta.mat,phi.mat=phi.mat,sigma.vec=sigma.vec,
                                            hparam.draw=hparam.draw,alpha=alpha,beta=beta,psi=psi)
      }

      ## Save parameters to output file if requested
      if(all(!is.null(file.out),pos %% 100 == 0)){
        wrm.out <- list(ave.param.list=ave.param.list,final.param.list=final.param.list,ntopics=ntopics)
        if(save.burnin){wrm.out$burnin.param.list <- burnin.param.list}
        save(wrm.out,file=file.out)
      }
      
    } else {
      # Otherwise only save burnin draws if requested
      if(save.burnin){
        if(i==1){
          burnin.param.list <- setup.final.param.list(mu.mat=mu.mat,theta.mat=theta.mat,
                                                      phi.mat=phi.mat,doc.ids=doc.ids,word.ids=word.ids,
                                                      hparam.draw=hparam.draw,alpha=alpha,beta=beta,psi=psi,
                                                      ndraws.store=burnin,D=D,V=V,ntopics=ntopics,
                                                      nwords.trace=nwords.trace,ndocs.trace=ndocs.trace)
          burnin.param.list <- update.final.param.list(pos=1,mu.mat=mu.mat,theta.mat=theta.mat,
                                                       phi.mat=phi.mat,final.param.list=burnin.param.list,
                                                       hparam.draw=hparam.draw,alpha=alpha,beta=beta,psi=psi)
        } else {
          burnin.param.list <- update.final.param.list(pos=i,mu.mat=mu.mat,theta.mat=theta.mat,
                                                       phi.mat=phi.mat,final.param.list=burnin.param.list,
                                                       hparam.draw=hparam.draw,alpha=alpha,beta=beta,psi=psi)
        }
      }
    }

    ## Print iteration time
    if(verbose){
      t1 <- proc.time()[3]
      time.iter <- t1-t0
      cat(sprintft("Iteration %s took %0.2f seconds\n",i,time.iter))
    }
    
  }

  ## Return posterior draws of lambda and theta matrices
  out.list <- list(ave.param.list=ave.param.list,final.param.list=final.param.list,
                   ntopics=ntopics,burnin=burnin,iter=iter)

  ## Add in burnin draws if requested
  if(all(save.burnin,burnin>0)){out.list$burnin.param.list <- burnin.param.list}

  return(out.list)

}


## Function to draw array of multinomials with different prob
## vector for each row
multi.draw <- function(Q,wc.obs,rate.mat){
  out <- sapply(1:Q,function(q){rmultinom(n=1,size=wc.obs[q],
                                          prob=rate.mat[,q])})
  return(t(out))
}

## Function to initialize array of multinomials from uniform
## distribution
multi.init.draw <- function(obs.count,ntopics){
  out <- sapply(obs.count,rmultinom,n=1,prob=rep(1,ntopics))
  return(t(out))
}

## Function to draw augmented word counts
draw.wc.aug <- function(lambda.mat,theta.mat,Q,wc.obs,
                        d.index,f.index){
  ## Compute wfdk rates based on lambda and theta matrices
  rate.mat <- mapply(function(d,f){theta.mat[d,]*lambda.mat[f,]},
                     d=d.index,f=f.index)
  
  ## Draw augmented word counts
  wc.aug <- multi.draw(Q=Q,wc.obs=wc.obs,rate.mat=rate.mat)

  return(wc.aug)
}


lambda.mat.draw <- function(wfk.mat,doc.length.vec,theta.mat,
                            psi,beta,ntopics){
  convo.mat <- wfk.mat + beta
  rate.vec <- psi + colSums(doc.length.vec*theta.mat)
  out <- t(apply(convo.mat,1,gamma.mat.draw,rate.vec=rate.vec,
                 ntopics=ntopics))
  return(out)
}

## theta.mat.draw <- function(wdk.mat,lambda.mat,
##                            alpha,ntopics){
##   convo.mat <- wdk.mat + alpha
##   t.vec <- colSums(lambda.mat)
##   out <- t(apply(convo.mat,1,gamma.mat.draw,rate.vec=1,
##                ntopics=ntopics,normalize=TRUE))
##   return(out)
## }

gamma.mat.draw <- function(convo.vec,rate.vec,ntopics,normalize=FALSE){
  out <- rgamma(n=ntopics,shape=convo.vec,rate=rate.vec)
  if(normalize){out <- out/sum(out)}
  return(out)
}

## Function to implement independence chain Metropolis correction for thetas
metro.indep.correct.theta <- function(theta.cand,theta.old,t.vec,doc.length){
  ## Log of Metropolis ratio
  log.r <- doc.length*(sum(theta.old*t.vec) - sum(theta.cand*t.vec))
  ## Compare against log uniform (also check for illegal values)
  if(any(is.nan(log.r),is.na(log.r))){accept <- FALSE
    } else {accept <- log.r > log(runif(1))}
  ## Return candidate or old draw depending on decision
  if(accept) {theta <- theta.cand} else {theta <- theta.old}
  return(theta)
}

## Function to convert a matrix into a list of its rows
mat2list <- function(mat){
  out <- lapply(seq_len(nrow(x)),function(i){x[i,]})
  return(out)
}

## Function to implement independence chain Metropolis sampler for thetas
theta.mat.draw <- function(wdk.mat,lambda.mat,theta.mat,
                           alpha,ntopics,doc.length.vec,
                           ndraw=1,metro.correct=TRUE){
  ndocs <- length(doc.length.vec)
  convo.mat <- wdk.mat + alpha
  t.vec <- colSums(lambda.mat)

  if(metro.correct){
    for(j in 1:ndraw){
      ## Draw candidate from proposal
      theta.cand.mat <- t(apply(convo.mat,1,gamma.mat.draw,rate.vec=1,
                                ntopics=ntopics,normalize=TRUE))
      theta.mat <- t(sapply(1:ndocs,function(i){
        return(metro.indep.correct.theta(theta.cand=theta.cand.mat[i,],
                                         theta.old=theta.mat[i,],
                                         t.vec=t.vec,doc.length=doc.length.vec[i]))
      }))
    }
                                        # Reinstate rownames for theta matrix
    rownames(theta.mat) <- rownames(theta.cand.mat)
  } else {
    ## Else if Metropolis correction not wanted, get single draw from proposal
    theta.mat <- t(apply(convo.mat,1,gamma.mat.draw,rate.vec=1,
                         ntopics=ntopics,normalize=TRUE))
  }
  return(theta.mat)
}


## Function to compute rate for each f,d,k word
get.fdk.rate <- function(dframe,lambda.mat,theta.mat){
  f <- as.character(dframe$f[1])
  d <- as.character(dframe$d[1])
  dframe$rate <- lambda.mat[f,]*theta.mat[d,]
  return(dframe)
}

## Function to calculate phis as a function of the lambdas
get.phi.mat <- function(lambda.mat){
  phi.mat <- t(apply(lambda.mat,1,function(row){row/sum(row)}))
  return(phi.mat)
}

## Function to set up list to store sample of posterior draws
setup.final.param.list <- function(mu.mat,theta.mat,phi.mat,doc.ids,word.ids,
                                   ndraws.store,D,V,ntopics,
                                   hparam.draw=FALSE,alpha=NULL,beta=NULL,psi=NULL,
                                   nwords.trace=NULL,ndocs.trace=NULL){
  
  if(!is.null(nwords.trace)){
    ## Pick random sample of words to store if requested
    word.ids.trace <- sample(x=word.ids,size=nwords.trace,replace=FALSE)
  } else {
    nwords.trace <- V
    word.ids.trace <- word.ids
  }
  
  ## Pick random sample of docs to store if requested
  if(!is.null(ndocs.trace)){
    doc.ids.trace <- sample(x=doc.ids,size=ndocs.trace,replace=FALSE)
  } else {
    ndocs.trace <- D
    doc.ids.trace <- doc.ids
  }

  final.param.list <- list()
  final.param.list$mu.mat <- array(NA,dim=c(nwords.trace,ntopics,ndraws.store),
                                       dimnames=list(word.ids.trace,1:ntopics,NULL))
  final.param.list$phi.mat <- array(NA,dim=c(nwords.trace,ntopics,ndraws.store),
                                       dimnames=list(word.ids.trace,1:ntopics,NULL))
  final.param.list$theta.mat <- array(NA,dim=c(ndocs.trace,ntopics,ndraws.store),
                                       dimnames=list(doc.ids.trace,1:ntopics,NULL))

  if(hparam.draw){
    final.param.list$alpha <- rep(NA,ndraws.store)
    final.param.list$beta <- rep(NA,ndraws.store)
    final.param.list$psi <- rep(NA,ndraws.store)
  }
  
  return(final.param.list)
}

## Function to update sample of posterior draws
update.final.param.list <- function(pos,mu.mat,theta.mat,phi.mat,
                                    final.param.list,
                                    hparam.draw=FALSE,alpha=NULL,
                                    beta=NULL,psi=NULL){
  
  words.trace <- dimnames(final.param.list$mu.mat)[[1]]
  final.param.list$mu.mat[,,pos] <- mu.mat[words.trace,]
  final.param.list$phi.mat[,,pos] <- phi.mat[words.trace,]
  docs.trace <- dimnames(final.param.list$theta.mat)[[1]]
  final.param.list$theta.mat[,,pos] <- theta.mat[docs.trace,]

  if(hparam.draw){
    final.param.list$alpha[pos] <- alpha
    final.param.list$beta[pos] <- beta
    final.param.list$psi[pos] <- psi
  }

  return(final.param.list)
}


## Function to update posterior expectation
update.ave.params <- function(pos,ave.param.list,mu.mat,theta.mat,
                              sigma.vec,phi.mat,
                              hparam.draw=FALSE,alpha=NULL,
                              beta=NULL,psi=NULL){

  ## Get relative weights
  weight.current <- (pos-1)/pos
  weight.new <- 1/pos

  ## Update averages with new draw according to weights
  ave.param.list$mu.mat <- weight.current*ave.param.list$mu.mat +
    weight.new*mu.mat
  ave.param.list$phi.mat <- weight.current*ave.param.list$phi.mat +
    weight.new*phi.mat
  ave.param.list$theta.mat <- weight.current*ave.param.list$theta.mat +
    weight.new*theta.mat
  ave.param.list$sigma.vec <- weight.current*ave.param.list$sigma.vec +
    weight.new*sigma.vec

  ## Same for hparams if requested
  if(hparam.draw){
    ave.param.list$alpha <- weight.current*ave.param.list$alpha +
      weight.new*alpha
    ave.param.list$beta <- weight.current*ave.param.list$beta +
      weight.new*beta
    ave.param.list$psi <- weight.current*ave.param.list$psi +
      weight.new*psi
  }

  return(ave.param.list)
}



sprintft <- function(x,...){return(paste0(date(),": ",sprintf(x,...)))}
