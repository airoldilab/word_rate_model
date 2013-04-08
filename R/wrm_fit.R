library("plyr")
library("foreach")
library("doParallel")
#workers <- makeCluster(4) # My computer has 4 cores
#registerDoParallel(workers)

# Function to initialize parallel computation
init.parallel <- function(cores){
  #workers <- makeCluster(cores)
  #registerDoParallel(workers)
  registerDoParallel(cores)
}


wrm.fit <- function(
             # data.frame with columns d,f,count
             wc,
             # Vector of document lengths
             doc.length.vec,
             # Number of topics
             ntopics,
             # Hyperparameters
             beta=1,alpha=0.05,psi=1,
             # Iterations
             iter=100,burnin=0,
             # Parallel computation?
             parallel=FALSE,
             verbose=FALSE){

  # Model inputs
  doc.ids <- unique(wc$d)
  word.ids <- unique(wc$f)
  D <- length(doc.ids)
  V <- length(word.ids)

  # Initalize augmented word count array
  wc$id <- rownames(wc)
  wc.aug <- ddply(.data=wc,.variables="id",.fun=multi.init.draw,
                  count.col="count",ntopics=ntopics,.parallel=parallel)
  wc.aug$rate <- 1

  # Initialize theta matrix
  theta.mat <- matrix(1/ntopics,ncol=ntopics,nrow=D)

  for(i in 1:iter){

    if(verbose){
      print(i)
      # Start time
      t0 <-  proc.time()[3]
    }

    # Get new wfk VxK matrix
    wfk.mat <- daply(.data=wc.aug,.variables=c("f","k"),
                    .fun=acount.sum,.parallel=parallel)
    # Get new wdk DxK matrix
    wdk.mat <- daply(.data=wc.aug,.variables=c("d","k"),
                    .fun=acount.sum,.parallel=parallel)
    
    # Draw the rates as function of wfk.vec
    lambda.mat <- lambda.mat.draw(wfk.mat=wfk.mat,psi=psi,
                                  doc.length.vec=doc.length.vec,
                                  theta.mat=theta.mat,beta=beta,
                                  ntopics=ntopics)
    theta.mat <- theta.mat.draw(wdk.mat=wdk.mat,theta.mat=theta.mat,alpha=alpha,
                                ntopics=ntopics)
    
    # Compute wfdk rates based on lambda and theta matrices
    # Get new wfk VxK matrix
    wc.aug <- ddply(.data=wc.aug,.variables=c("f","d"),
                    .fun=get.fdk.rate,lambda.mat=lambda.mat,
                    theta.mat=theta.mat,.parallel=parallel)
    
    # Redraw augmented word counts
    wc.aug <- ddply(.data=wc.aug,.variables=c("d","f"),
                    .fun=multi.draw,prob.col="rate",
                    count.col="count",aug.count.col="acount",
                    .parallel=parallel)

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
multi.draw <- function(dframe,prob.col,count.col,aug.count.col){
  dframe[,aug.count.col] <-
    rmultinom(n=1,size=dframe[,count.col][1],prob=dframe[,prob.col])
  return(dframe)
}

multi.init.draw <- function(dframe,ntopics,count.col){
  out.multi <- rmultinom(n=1,size=dframe[,count.col][1],prob=rep(1,ntopics))
  out <- as.data.frame(cbind(dframe,k=1:ntopics,acount=out.multi))
  return(out)
}

acount.sum <- function(dframe){
  out <- sum(dframe$acount)
  return(out)
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
