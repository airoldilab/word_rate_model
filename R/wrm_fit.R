
wrm.fit <- function(
             # Vector of obs word counts
             wc.obs,
             # Vector of doc ids for word counts
             d.index,
             # Vector of word ids for word counts
             f.index,
             # Labeled vector of document lengths
             # Length 'D' and 
             doc.length.vec,
             # Number of topics
             ntopics,
             # Hyperparameters
             beta=1,alpha=0.05,psi=1,
             # Iterations
             iter=100,burnin=0,
             verbose=FALSE){

  # Model inputs
  doc.ids <- unique(d.index)
  word.ids <- unique(f.index)
  D <- length(doc.ids)
  V <- length(word.ids)
  Q <- length(d.index)
  d.index <- factor(d.index)
  f.index <- factor(f.index)

  # Initalize augmented word count array
  wc.aug <- multi.init.draw(obs.count=wc.obs,ntopics=ntopics)

  # Initialize theta matrix
  theta.mat <- matrix(1/ntopics,ncol=ntopics,nrow=D)

  # Get linear operator for topic-specific counts
  A.d <- t(sparse.model.matrix(~ factor(d.index) + 0))
  rownames(A.d) <- levels(d.index)
  A.f <- t(sparse.model.matrix(~ factor(f.index) + 0))
  rownames(A.f) <- levels(f.index)

  for(i in 1:iter){

    if(verbose){
      print(i)
      # Start time
      t0 <-  proc.time()[3]
    }

    # Get new wfk VxK matrix
    wfk.mat <- A.f%*%wc.aug
    # Get new wdk DxK matrix
    wdk.mat <- A.d%*%wc.aug
    
    # Get VxK matrix of rates from wfk.mat
    lambda.mat <- lambda.mat.draw(wfk.mat=wfk.mat,psi=psi,
                                  doc.length.vec=doc.length.vec,
                                  theta.mat=theta.mat,beta=beta,
                                  ntopics=ntopics)
    # Get DxK matrix of memberships from wdk.mat
    theta.mat <- theta.mat.draw(wdk.mat=wdk.mat,theta.mat=theta.mat,
                                alpha=alpha,ntopics=ntopics)
    
    # Compute wfdk rates based on lambda and theta matrices
    rate.mat <- mapply(function(d,f){theta.mat[d,]*lambda.mat[f,]},
                       d=d.index,f=f.index)
    
    # Redraw augmented word counts
    wc.aug <- multi.draw(Q,wc.obs,rate.mat)

    # Update posterior mean
    if(i > burnin){
      if(i==(burnin + 1)){ave.param.list <- list(lambda.mat=lambda.mat,theta.mat=theta.mat)
      } else {ave.param.list <- update.ave.params(i=1,ave.param.list=ave.param.list,
                                                  lambda.mat=lambda.mat,theta.mat=theta.mat,
                                                  burnin=burnin)
            }
    }

    # Print iteration time
    if(verbose){
      t1 <- proc.time()[3]
      time.iter <- round(t1-t0,3)
      cat(sprintft("Iteration %s took %0.2f seconds\n",i,time.iter))
    }
    

  }

  # Return posterior draws of lambda and theta matrices
  out.list <- ave.param.list

  return(out.list)

}


# Function to draw 
multi.draw <- function(Q,wc.obs,rate.mat){
  out <- sapply(1:Q,function(q){rmultinom(n=1,size=wc.obs[q],
                                          prob=rate.mat[,q])})
  return(t(out))
}

multi.init.draw <- function(obs.count,ntopics){
  out <- sapply(obs.count,rmultinom,n=1,prob=rep(1,ntopics))
  return(t(out))
}


lambda.mat.draw <- function(wfk.mat,doc.length.vec,theta.mat,
                            psi,beta,ntopics){
  convo.mat <- wfk.mat + beta
  rate.vec <- psi + colSums(doc.length.vec*theta.mat)
  out <- t(apply(convo.mat,1,gamma.mat.draw,rate.vec=rate.vec,
                 ntopics=ntopics))
  return(out)
}

theta.mat.draw <- function(wdk.mat,theta.mat,alpha,ntopics){
  convo.mat <- wdk.mat + alpha
  rate.vec <- colSums(theta.mat)
  out <- t(apply(convo.mat,1,gamma.mat.draw,rate.vec=rate.vec,
               ntopics=ntopics,normalize=TRUE))
  return(out)
}

gamma.mat.draw <- function(convo.vec,rate.vec,ntopics,normalize=FALSE){
  out <- rgamma(n=ntopics,shape=convo.vec,rate=rate.vec)
  if(normalize){out <- out/sum(out)}
  return(out)
}


# Function to compute rate for each f,d,k word
get.fdk.rate <- function(dframe,lambda.mat,theta.mat){
  f <- as.character(dframe$f[1])
  d <- as.character(dframe$d[1])
  dframe$rate <- lambda.mat[f,]*theta.mat[d,]
  return(dframe)
}


# Function to update posterior expectation of parameters
update.ave.params <- function(i,ave.param.list,lambda.mat,theta.mat,burnin){

  # Get relative weights
  pos <- i-burnin
  weight.current <- (pos-1)/pos
  weight.new <- 1/pos
  
  ave.param.list$lambda.mat <- weight.current*ave.param.list$lambda.mat +
    weight.new*lambda.mat
  ave.param.list$theta.mat <- weight.current*ave.param.list$theta.mat +
    weight.new*theta.mat

  return(ave.param.list)
}

sprintft <- function(x,...){return(paste(date(),": ",sprintf(x,...),sep=""))}
