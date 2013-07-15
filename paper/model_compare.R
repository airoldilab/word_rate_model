## Script to compare output of DWR and LDA models on quantitative metrics
library("xtable")
source("../R/wrm_analyze.R")
source("../R/wrm_fit.R")

## Folders were output is stored
output.dir <- "/n/airoldifs2/lab/jbischof/word_rate_output/"
plot.output.dir <- paste0(output.dir,"plots/")
sum.output.dir <- paste0(output.dir,"topic_sum/")
lda.output.dir <- paste0(output.dir,"lda_output/")

## Vector of topics used for each model
ntopics.vec <- as.character(c(10,25,50,100))
##ntopics.vec <- as.character(c(10,25,50))
iter.use.vec <- rep(1000,length(ntopics.vec))
names(iter.use.vec) <- ntopics.vec

## Load vector of marginal word counts
file.margwc <- paste0(output.dir,"ap_margwc.txt")
margwc.nosort <- read.table(file.margwc,row.names=1)
margwc.sort <- margwc.nosort[order(as.numeric(rownames(margwc.nosort))),,drop=FALSE]
margwc <- margwc.sort[,1]
names(margwc) <- rownames(margwc.sort)

## Load in vocab vector
vocab.file <- paste0(output.dir,"vocab.txt")
vocab <- read.table(file=vocab.file,colClasses="character")[,1]
names(vocab) <- 1:length(vocab) - 1


## Load matrices of word-topic rates
## DTR model
dtr.rate.list <- list()
dtr.trate.list <- list()
dtr.exc.list <- list()
dtr.frex.list <- list()
dtr.overall.rate.list <- list()
dtr.alpha <- c()
dtr.beta <- c()
dtr.psi <- c()
## dtr.frex.norm.list <- list()
for(ntopics in ntopics.vec){
  iter.use <- iter.use.vec[ntopics]
  output.file <- paste0(output.dir,"run_k",ntopics,"_i",
                        iter.use,"/wrm_out.RData")
  ## Load in output object
  load(output.file)
  dtr.trate.list[[ntopics]] <- wrm.out$ave.param.list$sigma.vec
  dtr.rate.list[[ntopics]] <- wrm.out$ave.param.list$mu.mat
  dtr.exc.list[[ntopics]] <- wrm.out$ave.param.list$phi.mat
  dtr.overall.rate.list[[ntopics]] <- wrm.out$ave.param.list$sigma.vec
  ## Calculate FREX scores
  dtr.frex.list[[ntopics]] <- get.word.loadings(wrm.out=wrm.out,
                                                type="frex",
                                                weight.freq=0.5)
  ## dtr.frex.norm.list[[ntopics]] <- apply(dtr.frex.list[[ntopics]],1,
  ##                                        function(col){col/sum(col)})
  # Examine hyperparameters
  dtr.alpha[ntopics] <- wrm.out$ave.param.list$alpha
  dtr.beta[ntopics] <- wrm.out$ave.param.list$beta
  dtr.psi[ntopics] <- wrm.out$ave.param.list$psi
}
dtr.log.frex.list <- lapply(dtr.frex.list,log)
dtr.log.exc.list <- lapply(dtr.exc.list,log)

## LDA model
lda.rate.list <- list()
lda.exc.list <- list()
lda.frex.list <- list()
for(ntopics in ntopics.vec){
  output.file <- paste0(lda.output.dir,"run_k",ntopics,"/final.beta")
  ## Read in output object
  lda.out.vec <- scan(output.file)
  lda.rate.mat <- matrix(lda.out.vec,byrow=FALSE,
                         ncol=as.numeric(ntopics))
  lda.rate.list[[ntopics]] <- lda.rate.mat
  ## Calculate implied exclusivity and frex scores for lda fit
  ## Won't necessarily be possible to compute logit since
  ## zeros and ones are possible
  lda.exc.mat <- exp(t(apply(lda.rate.mat,1,norm.log.prob)))
  #lda.exc.mat <- get.phi.mat(exp(lda.rate.mat))
  lda.frex.mat <- get.word.loadings(rate.mat=lda.rate.mat,
                                    exc.mat=lda.exc.mat,
                                    type="frex",weight.freq=0.5)
  lda.exc.list[[ntopics]] <- lda.exc.mat
  lda.frex.list[[ntopics]] <- lda.frex.mat
}

## Task 0: examine DTR fit

## Comparing total across for different number of topics
## How does it compare to the marginal counts for the words?
ntopics.plot <- "10"
plot(log(margwc),log(dtr.trate.list[["10"]]))
plot(log(margwc),log(dtr.trate.list[["25"]]))
plot(log(margwc),log(dtr.trate.list[["50"]]))
plot(log(margwc),log(dtr.trate.list[["100"]]))
plot(log(dtr.trate.list[["10"]]),log(dtr.trate.list[["100"]]))

# Look at evolution of Dirichlet concentration parameter as change # of topics
hparam.table <- rbind(dtr.alpha,dtr.beta)
xtable(hparam.table,digits=3)

# Show regularization pattern by plotting exc against overall rate
ntopics.use <- c("10","100")
file.png <- paste0(plot.output.dir,"reg_demo.png")
png(file.png,width=14,height=7,units="in",res=150)
par(mfrow=c(1,2))
for(ntopics in ntopics.use){
  plot(log(dtr.overall.rate.list[[ntopics]]) - log(as.numeric(ntopics)),
       logit(dtr.exc.list[[ntopics]][,1]),cex=0.5,
       xlab=expression(paste("Overall rate: log(",sigma[f],")")),
       ylab=expression(paste("Exclusivity for topic 1: logit(",phi[f1],")")),
       main=paste(ntopics,"topics"))
  abline(h=logit(1/as.numeric(ntopics)),col="red",lwd=2)
  legend(x="topleft",legend="1/K baseline",col="red",lwd=2)
}
dev.off()


## Task 1: Find top-loading words in each topic across models and summary types
models <- c("dtr","lda")
nwords.sum <- 10
for (model in models){

  ## Get exc and freq lists for model
  exc.list <- get(paste0(model,".exc.list"))
  rate.list <- get(paste0(model,".rate.list"))

  for(ntopics in ntopics.vec){
    exc.mat <- exc.list[[ntopics]]
    rate.mat <- rate.list[[ntopics]]
    rownames(exc.mat) <- rownames(rate.mat) <- names(vocab)
    frex.sum <- get.top.words(rate.mat=rate.mat,exc.mat=exc.mat,
                              n.get=nwords.sum,vocab=vocab,type="frex",weight.freq=0.5)
    freq.sum <- get.top.words(rate.mat=rate.mat,n.get=nwords.sum,
                              vocab=vocab,type="freq",weight.freq=0.5)
    
    frex.file.out <- paste0(sum.output.dir,model,"_",ntopics,"_frex_ap_sum.txt")
    freq.file.out <- paste0(sum.output.dir,model,"_",ntopics,"_freq_ap_sum.txt")
    write.table(t(frex.sum),file=frex.file.out,sep=" ",quote=FALSE,
                row.names=1:ntopics,col.names=FALSE)
    write.table(t(freq.sum),file=freq.file.out,sep=" ",quote=FALSE,
                row.names=1:ntopics,col.names=FALSE)
  }
}


## Task 2: compare variance and entropy of exc vectors as a function of
## marginal word frequency
## Since only rate stable for LDA in log space, var only works for rates
dtr.rate.var <- lapply(dtr.rate.list,topic.dist.var)
lda.rate.var <- lapply(lda.rate.list,topic.dist.var)
dtr.rate.ent <- lapply(dtr.rate.list,topic.dist.entropy,logp=TRUE)
lda.rate.ent <- lapply(lda.rate.list,topic.dist.entropy,logp=TRUE)
dtr.exc.var <- lapply(dtr.exc.list,topic.dist.var)
lda.exc.var <- lapply(lda.exc.list,topic.dist.var)
dtr.exc.ent <- lapply(dtr.exc.list,topic.dist.entropy,logp=FALSE)
lda.exc.ent <- lapply(lda.exc.list,topic.dist.entropy,logp=FALSE)
dtr.exc.max <- lapply(dtr.exc.list,topic.dist.max)
lda.exc.max <- lapply(lda.exc.list,topic.dist.max)
dtr.frex.max <- lapply(dtr.frex.list,topic.dist.max)
lda.frex.max <- lapply(lda.frex.list,topic.dist.max)

## Create plots of LDA v. FREX exclusivity measurements
## Can also try the variance or comparison to Eisenstein

## Consider plotting bin averages instead (or loess)
metrics <- c("exc.max","rate.var","exc.ent")
ylabels <- c("Maximum logit exclusivity","Variance of log word rates",
             "Entropy of exclusivity distribution")
filename.metrics <- c("exc_max","rate_var","exc_ent")
names(ylabels) <- names(filename.metrics) <- metrics
for(ntopics.plot in ntopics.vec){
  for(metric in metrics){
    lda.metric <- get(paste0("lda.",metric))[[ntopics.plot]]
    if(metric=="exc.max"){
      lda.metric <- logit(lda.metric) 
      lda.metric[lda.metric==Inf] <- max(lda.metric[!lda.metric==Inf])
    } 
    dtr.metric <- get(paste0("dtr.",metric))[[ntopics.plot]]
    if(metric=="exc.max"){dtr.metric <- logit(dtr.metric)} 
    x.plot <- log(margwc)
    ##x.plot <- log(dtr.overall.rate.list[[ntopics.plot]])
    lda.loess.mat <- curve.loess(x=x.plot,y=lda.metric,degree=0,span=0.2)
    dtr.loess.mat <- curve.loess(x=x.plot,y=dtr.metric,degree=0,span=0.2)
    file.png <- paste0(plot.output.dir,filename.metrics[metric],
                       "_",ntopics.plot,"_comp.png")
    png(file.png,width=14,height=6,units="in",res=150)
    par(mfrow=c(1,2),mar=c(4,4,2,2))
    plot(x.plot,lda.metric,cex=0.5,xlab="Log marginal word count",
         ylab=ylabels[metric])
    lines(lda.loess.mat,col="red",lwd=2)
    plot(x.plot,dtr.metric,cex=0.5,xlab="Log marginal word count",
         ylab=ylabels[metric])
    lines(dtr.loess.mat,col="red",lwd=2)
    dev.off()
  }
}
## Need FREX plots for LDA as well to see (non)shrinkage pattern

## Task 2: Calculate similiarity of word-topic scores
## Need to ensure that frex scores sum to one to that is actually a probability measure
## nwords.sim <- "all"
## dtr.kl.mat <- lapply(dtr.log.frex.list,FUN=comp.dist.all,fun.compare=kl.dist,
##                      nwords=nwords.use)
## lda.kl.mat <- lapply(lda.rate.list,comp.dist.all,fun.compare=kl.dist,
##                      nwords=nwords.use)
## dtr.hel.mat <- lapply(dtr.log.frex.list,comp.dist.all,fun.compare=hel.dist,
##                       nwords=nwords.use)
## lda.hel.mat <- lapply(lda.rate.list,comp.dist.all,fun.compare=hel.dist,
##                       nwords=nwords.use)

## dtr.exc.kl <- lapply(dtr.log.exc.list,FUN=comp.dist.all,fun.compare=kl.dist,
##                      nwords=nwords.use)
## dtr.exc.hel <- lapply(dtr.log.exc.list,FUN=comp.dist.all,fun.compare=hel.dist,
##                       nwords=nwords.use)

## sapply(dtr.exc.kl,mean)
## sapply(dtr.kl.mat,mean)
## sapply(lda.kl.mat,mean)

## sapply(dtr.exc.hel,mean)
## sapply(dtr.hel.mat,mean)
## sapply(lda.hel.mat,mean)

## Rank correlation metric
nwords.sim <- "all"
dtr.frex.cor <- sapply(dtr.frex.list,rank.cor.dist.mat,
                       nwords=nwords.sim,ave=TRUE)
dtr.rate.cor <- sapply(dtr.rate.list,rank.cor.dist.mat,
                       nwords=nwords.sim,ave=TRUE)
dtr.exc.cor <- sapply(dtr.exc.list,rank.cor.dist.mat,
                      nwords=nwords.sim,ave=TRUE)
lda.frex.cor <- sapply(lda.frex.list,rank.cor.dist.mat,
                       nwords=nwords.sim,ave=TRUE)
lda.rate.cor <- sapply(lda.rate.list,rank.cor.dist.mat,
                       nwords=nwords.sim,ave=TRUE)
cor.mat <- rbind(dtr.frex.cor,dtr.rate.cor,dtr.exc.cor,lda.frex.cor,lda.rate.cor)
rownames(cor.mat) <- c("DTR FREX","DTR RATE","DTR EXC","LDA FREX","LDA RATE")
print(xtable(cor.mat,digits=3))


## Unique words metric
## Need to get this in terms of proportion of total possible unique words
nwords.vec <- c(10,25,50,100,250)
for(nwords in nwords.vec){
  dtr.frex.unique <- sapply(dtr.frex.list,unique.words.mat,
                            nwords=nwords)
  dtr.rate.unique <- sapply(dtr.rate.list,unique.words.mat,
                            nwords=nwords)
  dtr.exc.unique <- sapply(dtr.exc.list,unique.words.mat,
                            nwords=nwords)
  lda.frex.unique <- sapply(lda.frex.list,unique.words.mat,
                            nwords=nwords)
  lda.rate.unique <- sapply(lda.rate.list,unique.words.mat,
                            nwords=nwords)
  unique.mat <- rbind(dtr.frex.unique,dtr.rate.unique,dtr.exc.unique,lda.frex.unique,
                      lda.rate.unique)
  unique.prop.mat <- t(t(unique.mat)/(as.numeric(ntopics.vec)*nwords))
  rownames(unique.mat) <- c("DTR FREX","DTR RATE","DTR EXC","LDA FREX","LDA RATE")
  dimnames(unique.prop.mat) <- dimnames(unique.mat)
  print(nwords)
  #print(round(unique.mat,3))
  print(xtable(unique.prop.mat,digits=3))
}
