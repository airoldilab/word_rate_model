## Function to get word loadings in each topic

## Function to check convergence of Gibbs sampler
trace.wrm <- function(wrm.out,type="word",pos=1,burnin.draws=FALSE){

  ## Should burnin or post-burnin draws be plotted?
  if(burnin.draws){param.list <- wrm.out$burnin.param.list
  } else {param.list <- wrm.out$final.param.list}
  
  ## Grab trace of desired doc or word parameters
  if(type=="word"){
    trace.data <- param.list$mu.mat[pos,,]
  } else if (type=="doc"){
    trace.data <- param.list$theta.mat[pos,,]
  } else if (type=="exc"){
    trace.data <- param.list$phi.mat[pos,,]
  }

  ntopics <- nrow(trace.data)

  ## Plot traces of doc or word's parameters
  cols.use <- rainbow(ntopics)
  plot(trace.data[1,],type="l",xlab="Iteration",col=cols.use[1],
       ylim=c(min(trace.data),max(trace.data)),ylab=paste(type,"parameters"))
  for(j in 2:ntopics){
    lines(trace.data[j,],col=cols.use[j],type="l",xlab="Iteration")
  }
  
}

trace.hparam <- function(wrm.out,type="alpha",burnin.draws=FALSE){
  
  ## Should burnin or post-burnin draws be plotted?
  if(burnin.draws){param.list <- wrm.out$burnin.param.list
  } else {param.list <- wrm.out$final.param.list}
  
  if(type=="alpha"){
    trace.data <- param.list$alpha
  } else if(type=="beta"){
    trace.data <- param.list$beta
  } else if(type=="psi"){
    trace.data <- param.list$psi
  }

  ## Plot trace and hist of hyperparameter
  par(mfrow=c(1,2))
  plot(trace.data,type="l",xlab="Iteration",ylab=type,
       main=paste(type,"traceplot"))
  hist(trace.data,xlab=type,freq=FALSE,main=paste(type,"posterior histogram"))
}

## Load in logit function
logit <- function(x){log(x)-log(1-x)}

## Function to calculate frex score
frex.score <- function(freq.quant,exc.quant,weight.freq=0.5){
  score <- 1/(weight.freq/freq.quant + (1-weight.freq)/exc.quant)
  return(score)
}


get.word.loadings <- function(wrm.out=NULL,rate.mat=NULL,exc.mat=NULL,
                              type="frex",weight.freq=0.5){

  ## Retrieve relevant model output
  if(!is.null(wrm.out)){
    rate.mat <- wrm.out$ave.param.list$mu.mat
    exc.mat <- wrm.out$ave.param.list$phi.mat
  }

  ## Get cdf rankings for each word in each topic
  if(any(type=="freq",type=="frex")){
    rate.quant.mat <- apply(rate.mat,2,function(col){ecdf(col)(col)})
    if(type=="freq"){return(rate.quant.mat)}
  }
  if(any(type=="exc",type=="frex")){
    exc.quant.mat <- apply(exc.mat,2,function(col){ecdf(col)(col)})
    if(type=="exc"){return(exc.quant.mat)}
  }
  
  ## Calculate matrix of frex scores
  ntopics <- ncol(rate.mat)
  frex.mat <- sapply(1:ntopics,function(k){
    frex.score(rate.quant.mat[,k],exc.quant.mat[,k],weight.freq)})
  
  return(frex.mat)
}

## Function to get the top items in any vector
get.top.items <- function(vec,vec.labels,n.get){
  if(is.null(vec.labels)){vec.labels <- 1:length(vec)}
  vec.order <- rev(order(vec))
  pos.top <- vec.order[1:n.get]
  items.top <- vec.labels[pos.top]
  return(items.top)
}

## Function to find top-loading words in each topic
get.top.words <- function(wrm.out=NULL,rate.mat=NULL,exc.mat=NULL,
                          n.get=20,vocab=NULL,type="frex",weight.freq=0.5){

  ## Retrieve relevant model output
  if(!is.null(wrm.out)){
    rate.mat <- wrm.out$ave.param.list$mu.mat
    exc.mat <- wrm.out$ave.param.list$phi.mat
  }

  ## Get word ids
  if(exists("rate.mat")){
    word.ids <- rownames(rate.mat)
  } else {word.ids <- rownames(exc.mat)}
  
  ## Get desired word scores
  score.mat <- get.word.loadings(wrm.out=wrm.out,rate.mat=rate.mat,
                                 exc.mat=exc.mat,type=type,
                                 weight.freq=weight.freq)

  ## Get word ids for top loading items in each topic
  top.ids.mat <- t(apply(score.mat,2,get.top.items,
                         vec.labels=word.ids,n.get=n.get))

  ## Get word strings for top ids if given
  if(!is.null(vocab)){
    top.words.mat <- t(apply(top.ids.mat,2,function(col){
      vocab[as.character(col)]}))
  } else {top.words.mat <- top.ids.mat}

  return(top.words.mat)
}


## Get FREX plot for each topic
get.frex.plot <- function(wrm.out,vocab,
                          plot.dir,
                          lower.quant.cut.full=0.01,
                          upper.quant.zoom=0.95,
                          ## Plot parameters
                          res.plot=200,
                          size.inch=8){

  ## Get freq and exc word scores
  mu.mat <- wrm.out$ave.param.list$mu.mat
  logit.phi.mat <- logit(wrm.out$ave.param.list$phi.mat)

  ## Get word ids and ntopics
  word.ids <- rownames(wrm.out$ave.param.list$mu.mat)
  ntopics <- ncol(mu.mat)

  for(j in 1:ntopics){
    mu.vec <- mu.mat[,j]
    logit.phi.vec <- logit.phi.mat[,j]
    names(mu.vec) <- names(logit.phi.vec) <- rownames(mu.mat)
    
    ## Get quantiles of each dimension
    quant.mu <- quantile(mu.vec,probs=c(lower.quant.cut.full,
                                  upper.quant.zoom))
    quant.phi <- quantile(logit.phi.vec,probs=c(lower.quant.cut.full,
                                          upper.quant.zoom))

    
    ## Set up plot data

    ## Throw away very lowest points to keep full plot clean
    mu.keep.full <- mu.vec > quant.mu[1]
    phi.keep.full <- logit.phi.vec > quant.phi[1]
    index.keep.full <- apply(cbind(mu.keep.full,phi.keep.full),1,all)
    mu.vec.full <- mu.vec[index.keep.full]
    logit.phi.vec.full <- logit.phi.vec[index.keep.full]
    
    ## Get zoom plots - all points in top 5% of both dimensions
    mu.keep.zoom <- mu.vec > quant.mu[2]
    phi.keep.zoom <- logit.phi.vec > quant.phi[2]
    index.keep.zoom <- apply(cbind(mu.keep.zoom,phi.keep.zoom),1,all)
    mu.vec.zoom <- mu.vec[index.keep.zoom]
    logit.phi.vec.zoom <- logit.phi.vec[index.keep.zoom]
    ids.zoom <- word.ids[index.keep.zoom]
    labels.zoom <- vocab[ids.zoom]

    ## Get plot parameters
    lim.mu <- range(mu.vec.zoom)
    mar.mu <- (lim.mu[2]-lim.mu[1])*0.04
    lim.phi <- range(logit.phi.vec.zoom)
    mar.phi <- (lim.phi[2]-lim.phi[1])*0.04
    baseline <- logit(1/ntopics)

    ## Create zoom plot
    cex.plot <- 0.6
    title.plot <- paste("Upper 5% of FREX plot for topic",j)
    title.png <- paste(plot.dir,"fe_zoom_plot_",j,".png",sep="")
    png(title.png,width=size.inch,height=size.inch,units="in",res=res.plot)
    plot(mu.vec.zoom,logit.phi.vec.zoom,main=title.plot,
         ylab=expression(paste("Exclusivity: ",logit(phi[fk]))),
         xlab=expression(paste("Frequency: ",mu[fk])),cex=cex.plot,col="white")
    text(mu.vec.zoom, logit.phi.vec.zoom, labels = labels.zoom, adj = NULL,
         pos = NULL, offset = 0, vfont = NULL,
         cex = cex.plot, col = NULL, font = NULL)
    dev.off()
    
    title.plot <- paste("FREX plot for topic",j)
    title.png <- paste(plot.dir,"fe_plot_",j,".png",sep="")
    png(title.png,width=size.inch,height=size.inch,units="in",res=res.plot)
    plot(mu.vec.full,logit.phi.vec.full,main=title.plot,
         ylab=expression(paste("Exclusivity: ",logit(phi[fk]))),
       xlab=expression(paste("Frequency: ",mu[fk])),cex=0.5)
    rect(xleft=quant.mu[2],
         ybottom=quant.phi[2],
         ##xright=lim.mu[2]+mar.mu,
         ##ytop=lim.phi[2]+mar.phi,
         xright=max(mu.vec)+mar.mu,
         ytop=max(logit.phi.vec)+mar.phi,
         density = NULL, angle = 45,
       col = NULL, border = "red", lty = 2, lwd = 2)
 
    abline(h=baseline,col="red",lwd=2)
    legend(x="topleft",lwd=c(2,2),legend=c("1/K baseline","Top 5% words"),
           col="red",lty=c(1,2))
    dev.off()
  }
}


## Functions to compute concentration of word-topic scores as a
## function of marginal word rates

## Function to calculate variance of word-topic loadings
## Should be done on unrestricted scale
topic.dist.var <- function(score.mat){
  out.vec <- apply(score.mat,1,var)
  return(out.vec)
}

topic.dist.max <- function(score.mat){
  out.vec <- apply(score.mat,1,max)
  return(out.vec)
}

topic.dist.entropy <- function(score.mat,logp=TRUE){
  out.vec <- apply(score.mat,1,entropy.dist,logp=logp)
  return(out.vec)
}

## Function to ensure log probability dist is normalized
norm.log.prob <- function(log.prob.vec){
  out <- log.prob.vec - log(sum(exp(log.prob.vec)))
  return(out)
}
                             

## Function to calculate entropy of dist
entropy.dist <- function(prob.vec,logp=TRUE){
  if(logp==TRUE){
    log.prob.vec <- norm.log.prob(prob.vec)
    abs.prob.vec <- exp(log.prob.vec)
  } else {
    prob.vec <- prob.vec/sum(prob.vec)
    log.prob.vec <- log(prob.vec)
    log.prob.vec[log.prob.vec == -Inf] <- 0
    abs.prob.vec <- prob.vec
  }
  entropy <- -sum(abs.prob.vec*log.prob.vec)
  return(entropy)
}


# Distance comparison functions
# Coded in terms of log distributions

## Function to calculate KL divergence between two dists
kl.dist <- function(log.prob.vec1,log.prob.vec2){
  prob.vec1 <- exp(log.prob.vec1)
  kl.out <- sum((log.prob.vec1 - log.prob.vec2)*prob.vec1)
  return(kl.out)
}

## Function to calculate Hellinger distance between two dists
hel.dist <- function(log.prob.vec1,log.prob.vec2){
  # Make sure distributions normalized
  log.prob.vec1 <- log.prob.vec1 - log(sum(exp(log.prob.vec1)))
  sqrt.prob.vec1 <- sqrt(exp(log.prob.vec1))
  sqrt.prob.vec2 <- sqrt(exp(log.prob.vec2))
  hel.out <- sqrt(sum((sqrt.prob.vec1-sqrt.prob.vec2)^2))/sqrt(2)
  return(hel.out)
}

## Function to calculate average rank correlations for matrix of summaries
rank.cor.dist.mat <- function(score.mat,nwords="all",ave=TRUE){
  if(!nwords=="all"){score.mat <- score.mat[1:nwords,]}
  # Rank correlation matrix
  cor.mat <- cor(score.mat,method="spearman")
  # Vector of unique correlation metrics
  cor.vec <- cor.mat[lower.tri(cor.mat)]
  # Average unique correlation if requested
  if(ave){out <- mean(cor.vec)
        } else {out <- cor.vec}
  return(out)
}


## Function to calculate number of unique words in ranking
unique.words.mat <- function(score.mat,nwords){
  ## Get word ids for top loading items in each topic
  top.word.mat <- get.top.words(rate.mat=score.mat,n.get=nwords,
                                type="freq")
  # Get number of unique words among top ones
  nunique <- length(unique(as.vector(top.word.mat)))
  return(nunique)
}


## Function to calculate all pairwise distances between vectors in a matrix
## using one as the baseline
comp.dist.mat <- function(which.baseline,log.prob.mat,fun.compare){
  ncompare <- ncol(log.prob.mat) - 1
  log.prob.baseline <- log.prob.mat[,which.baseline]
  log.prob.compare <- log.prob.mat[,-which.baseline]
  out.vec <- apply(log.prob.compare,2,fun.compare,
                   log.prob.vec1=log.prob.baseline)
  return(out.vec)
}

## Function to alternate baseline dists for comp.dist.mat
comp.dist.all <- function(log.prob.mat,fun.compare,nwords="all"){
  ## Make sure log probabilities are normalized
  if(!nwords=="all"){log.prob.mat <- log.prob.mat[1:nwords,]}
  log.prob.mat <- apply(log.prob.mat,2,norm.log.prob)
  ntopics <- ncol(log.prob.mat)
  out.mat <- sapply(1:ntopics,comp.dist.mat,fun.compare=fun.compare,
                    log.prob.mat=log.prob.mat)
  return(out.mat)
}

## entropy.dist <- function(prob.vec){
##   log.prob <- log(prob.vec,base=2)
##   # Correct for any zero probabilities
##   which.zero <- which(sapply(log.prob,function(z){identical(z,-Inf)}))
##   log.prob[which.zero] <- 0
##   entropy <- sum(
## }


## Loess curve function for plots
curve.loess <- function(x,y,weights=NULL,prop.sample=NULL,...){
  if(is.null(weights)){weights <- rep(1,length(x))}
  ## Use only a sample of the data if requested for fitting
  if(!is.null(prop.sample)){
    n <- length(x)
    n.use <- n*prop.sample
    index.use <- sample.int(n=n,size=n.use)
    x.use <- x[index.use]
    y.use <- y[index.use]
    weights.use <- weights[index.use]
  } else {x.use <- x
          y.use <- y
          weights.use <- weights}
  loess.out <- loess(y.use ~ x.use,weights=weights.use,...)
  x.plot <- unique(x)
  curve.loess.out <- predict(loess.out,data.frame(x.use=x.plot))
  order.x <- order(x.plot)
  mat.loess.out <- cbind(x.plot[order.x],curve.loess.out[order.x])
  return(mat.loess.out)
}
