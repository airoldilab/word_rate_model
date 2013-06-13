## Script to compare output of DWR and LDA models on quantitative metrics

## Folders were output is stored
output.dir <- ""
lda.output.dir <- paste0(output.dir,"lda_output/")

## Vector of topics used for each model
ntopics.vec <- as.character(c(10,25,50,100))
iter.use.vec <- rep(1000,4)
names(iter.use.vec) <- ntopics.vec

## Load matrices of word-topic rates
## DWR model
dwr.rate.list <- list()
dwr.exc.list <- list()
dwr.frex.list <- list()
for(ntopics in ntopics.vec){
  iter.use <- iter.use.vec[ntopics]
  output.file <- paste0(output.dir,"run_k",ntopics,"i",iter.use,".RData")
  ## Load in output object
  load(output.file)
  dwr.rate.list[[ntopics]] <- wrm.out$ave.param.list$mu.mat
  dwr.exc.list[[ntopics]] <- wrm.out$ave.param.list$phi.mat
  ## Calculate FREX scores
  dwr.frex.list[[ntopics]] <- get.word.loadings(wrm.out=wrm.out,
                                                type="frex",
                                                weight.freq=0.5)
}

## LDA model
lda.rate.list <- list()
for(ntopics in ntopics.vec){
  iter.use <- iter.use.vec[ntopics]
  output.file <- paste0(output.dir,"run_k",ntopics,".RData")
  ## Read in output object
  lda.out.vec <- scan(output.file)
  lda.rate.list[[ntopics]] <- matrix(lda.out.vec,byrow=FALSE,
                                     ncol=as.numeric(ntopics))
  
}



