# Function to get word loadings in each topic

# Function to check convergence of Gibbs sampler
trace.wrm <- function(wrm.out,type="word",pos=1){
  
  # Grab trace of desired doc or word parameters
  if(type=="word"){
    trace.data <- wrm.out$final.param.list$mu.mat[pos,,]
  } else if (type=="doc"){
    trace.data <- wrm.out$final.param.list$theta.mat[pos,,]
  } else if (type=="exc"){
    trace.data <- wrm.out$final.param.list$phi.mat[pos,,]
  }

  ntopics <- nrow(trace.data)

  # Plot traces of doc or word's parameters
  cols.use <- rainbow(ntopics)
  plot(trace.data[1,],type="l",xlab="Iteration",col=cols.use[1],
       ylim=c(min(trace.data),max(trace.data)),ylab=paste(type,"parameters"))
  for(j in 2:ntopics){
    lines(trace.data[j,],col=cols.use[j],type="l",xlab="Iteration")
  }
  
}

trace.hparam <- function(wrm.out,type="alpha"){
  if(type=="alpha"){
    trace.data <- wrm.out$final.param.list$alpha
  } else if(type=="beta"){
    trace.data <- wrm.out$final.param.list$beta
  } else if(type=="psi"){
    trace.data <- wrm.out$final.param.list$psi
  }

  # Plot trace and hist of hyperparameter
  par(mfrow=c(1,2))
  plot(trace.data,type="l",xlab="Iteration",ylab=type,
       main=paste(type,"traceplot"))
  hist(trace.data,xlab=type,freq=FALSE,main=paste(type,"posterior histogram"))
}

# Load in logit function
logit <- function(x){log(x)-log(1-x)}

# Function to calculate frex score
frex.score <- function(freq.quant,exc.quant,weight.freq=0.5){
  score <- 1/(weight.freq/freq.quant + (1-weight.freq)/exc.quant)
  return(score)
}

get.word.loadings <- function(wrm.out,type="frex",weight.freq=0.5){

  # Retrieve relevant model output 
  mu.mat <- wrm.out$ave.param.list$mu.mat
  logit.phi.mat <- logit(wrm.out$ave.param.list$phi.mat)
  ntopics <- ncol(mu.mat)

  # Get cdf rankings for each word in each topic
  mu.quant.mat <- apply(mu.mat,2,function(col){ecdf(col)(col)})
  if(type=="freq"){return(mu.quant.mat)}
  phi.quant.mat <- apply(logit.phi.mat,2,function(col){ecdf(col)(col)})
  if(type=="exc"){return(phi.quant.mat)}

  # Calculate matrix of frex scores
  frex.mat <- sapply(1:ntopics,function(k){
    frex.score(mu.quant.mat[,k],phi.quant.mat[,k],weight.freq)})
  
  return(frex.mat)
}

# Function to get the top items in any vector
get.top.items <- function(vec,vec.labels,n.get){
  vec.order <- rev(order(vec))
  pos.top <- vec.order[1:n.get]
  items.top <- vec.labels[pos.top]
  return(items.top)
}

# Function to find top-loading words in each topic
get.top.words <- function(wrm.out,n.get,vocab,type="frex",weight.freq=0.5){

  # Get word ids
  word.ids <- rownames(wrm.out$ave.param.list$mu.mat)
  
  # Get desired word scores
  score.mat <- get.word.loadings(wrm.out=wrm.out,type=type,
                                 weight.freq=weight.freq)

  # Get word ids for top loading items in each topic
  top.ids.mat <- t(apply(score.mat,2,get.top.items,
                         vec.labels=word.ids,n.get=n.get))

  # Get word strings for top ids
  top.words.mat <- t(apply(top.ids.mat,2,function(col){
    vocab[as.character(col)]}))

  return(top.words.mat)
}


# Get FREX plot for each topic
get.frex.plot <- function(wrm.out,vocab,
                          plot.dir,
                          lower.quant.cut.full=0.01,
                          upper.quant.zoom=0.95,
                          # Plot parameters
                          res.plot=200,
                          size.inch=8){

  # Get freq and exc word scores
  mu.mat <- wrm.out$ave.param.list$mu.mat
  logit.phi.mat <- logit(wrm.out$ave.param.list$phi.mat)

  # Get word ids and ntopics
  word.ids <- rownames(wrm.out$ave.param.list$mu.mat)
  ntopics <- ncol(mu.mat)

  for(j in 1:ntopics){
    mu.vec <- mu.mat[,j]
    logit.phi.vec <- logit.phi.mat[,j]
    names(mu.vec) <- names(logit.phi.vec) <- rownames(mu.mat)
    
    # Get quantiles of each dimension
    quant.mu <- quantile(mu.vec,probs=c(lower.quant.cut.full,
                                  upper.quant.zoom))
    quant.phi <- quantile(logit.phi.vec,probs=c(lower.quant.cut.full,
                                          upper.quant.zoom))

    
    # Set up plot data

    # Throw away very lowest points to keep full plot clean
    mu.keep.full <- mu.vec > quant.mu[1]
    phi.keep.full <- logit.phi.vec > quant.phi[1]
    index.keep.full <- apply(cbind(mu.keep.full,phi.keep.full),1,all)
    mu.vec.full <- mu.vec[index.keep.full]
    logit.phi.vec.full <- logit.phi.vec[index.keep.full]
    
    # Get zoom plots - all points in top 5% of both dimensions
    mu.keep.zoom <- mu.vec > quant.mu[2]
    phi.keep.zoom <- logit.phi.vec > quant.phi[2]
    index.keep.zoom <- apply(cbind(mu.keep.zoom,phi.keep.zoom),1,all)
    mu.vec.zoom <- mu.vec[index.keep.zoom]
    logit.phi.vec.zoom <- logit.phi.vec[index.keep.zoom]
    ids.zoom <- word.ids[index.keep.zoom]
    labels.zoom <- vocab[ids.zoom]

    # Get plot parameters
    lim.mu <- range(mu.vec.zoom)
    mar.mu <- (lim.mu[2]-lim.mu[1])*0.04
    lim.phi <- range(logit.phi.vec.zoom)
    mar.phi <- (lim.phi[2]-lim.phi[1])*0.04
    baseline <- logit(1/ntopics)

    # Create zoom plot
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
         #xright=lim.mu[2]+mar.mu,
         #ytop=lim.phi[2]+mar.phi,
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
