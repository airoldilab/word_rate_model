## Script to compare output of DWR and LDA models on quantitative metrics
source("../R/wrm_analyze.R")
source("../R/wrm_fit.R")

## Folders were output is stored
output.dir <- "/n/airoldifs2/lab/jbischof/word_rate_output/"
lda.output.dir <- paste0(output.dir,"lda_output/")

## Vector of topics used for each model
ntopics.vec <- as.character(c(10,25,50,100))
iter.use.vec <- rep(1000,4)
names(iter.use.vec) <- ntopics.vec

## Load vector of marginal word counts
file.margwc <- paste0(output.dir,"ap_margwc.txt")
margwc.nosort <- read.table(file.margwc,row.names=1)
margwc.sort <- margwc.nosort[order(as.numeric(rownames(margwc.nosort))),,drop=FALSE]
margwc <- margwc.sort[,1]
names(margwc) <- rownames(margwc.sort)

## Load matrices of word-topic rates
## DWR model
dwr.rate.list <- list()
dwr.exc.list <- list()
dwr.frex.list <- list()
## dwr.frex.norm.list <- list()
for(ntopics in ntopics.vec){
  iter.use <- iter.use.vec[ntopics]
  output.file <- paste0(output.dir,"run_k",ntopics,"_i",
                        iter.use,"/wrm_out.RData")
  ## Load in output object
  load(output.file)
  dwr.rate.list[[ntopics]] <- wrm.out$ave.param.list$mu.mat
  dwr.exc.list[[ntopics]] <- wrm.out$ave.param.list$phi.mat
  ## Calculate FREX scores
  dwr.frex.list[[ntopics]] <- get.word.loadings(wrm.out=wrm.out,
                                                type="frex",
                                                weight.freq=0.5)
  ## dwr.frex.norm.list[[ntopics]] <- apply(dwr.frex.list[[ntopics]],1,
  ##                                        function(col){col/sum(col)})
}
dwr.log.frex.list <- lapply(dwr.frex.list,log)

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


## Task 1: compare variance and entropy of exc vectors as a function of
## marginal word frequency
## Since only rate stable for LDA in log space, var only works for rates
## Need to plot these as function of log(margwc) with loess smoother
dwr.rate.var <- lapply(dwr.rate.list,topic.dist.var)
lda.rate.var <- lapply(lda.rate.list,topic.dist.var)
dwr.rate.ent <- lapply(dwr.rate.list,topic.dist.entropy,logp=TRUE)
lda.rate.ent <- lapply(lda.rate.list,topic.dist.entropy,logp=TRUE)
dwr.exc.var <- lapply(dwr.exc.list,topic.dist.var)
lda.exc.var <- lapply(lda.exc.list,topic.dist.var)
dwr.exc.ent <- lapply(dwr.exc.list,topic.dist.entropy,logp=FALSE)
lda.exc.ent <- lapply(lda.exc.list,topic.dist.entropy,logp=FALSE)
dwr.exc.max <- lapply(dwr.exc.list,topic.dist.max)
lda.exc.max <- lapply(lda.exc.list,topic.dist.max)
dwr.frex.max <- lapply(dwr.frex.list,topic.dist.max)
lda.frex.max <- lapply(lda.frex.list,topic.dist.max)

## Task 2: Calculate similiarity of word-topic scores
## Need to ensure that frex scores sum to one to that is actually a probability measure
dwr.kl.mat <- lapply(dwr.log.frex.list,FUN=comp.dist.all,fun.compare=kl.dist)
lda.kl.mat <- lapply(lda.rate.list,comp.dist.all,fun.compare=kl.dist)
dwr.hel.mat <- lapply(dwr.log.frex.list,comp.dist.all,fun.compare=hel.dist)
lda.hel.mat <- lapply(lda.rate.list,comp.dist.all,fun.compare=hel.dist)
