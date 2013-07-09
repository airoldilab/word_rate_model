## Script to compare output of DWR and LDA models on quantitative metrics
source("../R/wrm_analyze.R")
source("../R/wrm_fit.R")

## Folders were output is stored
output.dir <- "/n/airoldifs2/lab/jbischof/word_rate_output/"
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

## Load matrices of word-topic rates
## DTR model
dtr.rate.list <- list()
dtr.trate.list <- list()
dtr.exc.list <- list()
dtr.frex.list <- list()
overall.rate.list <- list()
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
  overall.rate.list[[ntopics]] <- apply(exp(wrm.out$ave.param.list$mu.mat),1,sum)
  ## Calculate FREX scores
  dtr.frex.list[[ntopics]] <- get.word.loadings(wrm.out=wrm.out,
                                                type="frex",
                                                weight.freq=0.5)
  ## dtr.frex.norm.list[[ntopics]] <- apply(dtr.frex.list[[ntopics]],1,
  ##                                        function(col){col/sum(col)})
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
  lda.exc.mat <- get.phi.mat(exp(lda.rate.mat))
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

## Task 1: compare variance and entropy of exc vectors as a function of
## marginal word frequency
## Since only rate stable for LDA in log space, var only works for rates
## Need to plot these as function of log(margwc) with loess smoother
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
## Consider plotting bin averages instead (or lowess)
ntopics.plot <- "10"
lda.metric <- lda.exc.max[[ntopics.plot]]
dtr.metric <- dtr.exc.max[[ntopics.plot]]
x.plot <- log(margwc)
x.plot <- log(overall.rate.list[[ntopics.plot]])
lda.loess.mat <- curve.loess(x=x.plot,y=lda.metric)
dtr.loess.mat <- curve.loess(x=x.plot,y=dtr.metric)
par(mfrow=c(1,2))
plot(x.plot,lda.metric,cex=0.5)
lines(lda.loess.mat,col="red",lwd=2)
plot(x.plot,dtr.metric,cex=0.5)
lines(dtr.loess.mat,col="red",lwd=2)
## Need FREX plots for LDA as well to see (non)shrinkage pattern

## Task 2: Calculate similiarity of word-topic scores
## Need to ensure that frex scores sum to one to that is actually a probability measure
nwords.use <- "all"
dtr.kl.mat <- lapply(dtr.log.frex.list,FUN=comp.dist.all,fun.compare=kl.dist,
                     nwords=nwords.use)
lda.kl.mat <- lapply(lda.rate.list,comp.dist.all,fun.compare=kl.dist,
                     nwords=nwords.use)
dtr.hel.mat <- lapply(dtr.log.frex.list,comp.dist.all,fun.compare=hel.dist,
                      nwords=nwords.use)
lda.hel.mat <- lapply(lda.rate.list,comp.dist.all,fun.compare=hel.dist,
                      nwords=nwords.use)

dtr.exc.kl <- lapply(dtr.log.exc.list,FUN=comp.dist.all,fun.compare=kl.dist,
                     nwords=nwords.use)
dtr.exc.hel <- lapply(dtr.log.exc.list,FUN=comp.dist.all,fun.compare=hel.dist,
                      nwords=nwords.use)

sapply(dtr.exc.kl,mean)
sapply(dtr.kl.mat,mean)
sapply(lda.kl.mat,mean)

sapply(dtr.exc.hel,mean)
sapply(dtr.hel.mat,mean)
sapply(lda.hel.mat,mean)

## Rank correlation metric
dtr.frex.cor <- sapply(dtr.frex.list,rank.cor.dist.mat,
                       nwords="all",ave=TRUE)
dtr.rate.cor <- sapply(dtr.rate.list,rank.cor.dist.mat,
                       nwords="all",ave=TRUE)
dtr.exc.cor <- sapply(dtr.exc.list,rank.cor.dist.mat,
                      nwords="all",ave=TRUE)
lda.rate.cor <- sapply(lda.rate.list,rank.cor.dist.mat,
                       nwords="all",ave=TRUE)
cor.mat <- rbind(dtr.frex.cor,dtr.rate.cor,dtr.exc.cor,lda.rate.cor)
rownames(cor.mat) <- c("DTR FREX","DTR RATE","DTR EXC","LDA RATE")
print(round(cor.mat,4))


# Unique words metric
nwords.vec <- c(10,25,50,100,250)
for(nwords in nwords.vec){
  dtr.frex.unique <- sapply(dtr.frex.list,unique.words.mat,
                            nwords=nwords)
  dtr.rate.unique <- sapply(dtr.rate.list,unique.words.mat,
                            nwords=nwords)
  dtr.exc.unique <- sapply(dtr.exc.list,unique.words.mat,
                            nwords=nwords)
  lda.rate.unique <- sapply(lda.rate.list,unique.words.mat,
                            nwords=nwords)
  unique.mat <- rbind(dtr.frex.unique,dtr.rate.unique,dtr.exc.unique,lda.rate.unique)
  rownames(unique.mat) <- c("DTR FREX","DTR RATE","DTR EXC","LDA RATE")
  print(nwords)
  print(round(unique.mat,4))
}
