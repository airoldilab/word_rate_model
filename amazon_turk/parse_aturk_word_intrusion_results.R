## Load helper functions
source("parse_aturk_functions.R")

output.dir <- "/home/jbischof/Project_output/word_rate_model/aturk_output/"

# Inputs
ntopics.use <- c(10,25,50,100)
r <- 2
nmodels <- 3
prop.success.mat <- matrix(NA,ncol=nmodels,nrow=length(ntopics.use))
rownames(prop.success.mat) <- ntopics.use
n.trial.mat <- prop.success.mat
binom.ci.list <- list()

for(ntopics in ntopics.use){
  t <- ntopics
  ## Load in file results file
  main.dir <- "/home/jbischof/Project_output/word_rate_model/"
  input.dir <- paste0(main.dir,"aturk_input/")
  output.dir <- paste0(main.dir,"aturk_output/")
  infile.res <- paste0(output.dir,"wi_t",t,"_r",r,"_results.csv")
  tab.res <- read.csv(infile.res,header=TRUE)
  ## Only keep accepted assignments if there are duplicates
  hit.dups <- tab.res$HITId[duplicated(tab.res$HITId)]
  for (hit in hit.dups){
    which.remove <- which(tab.res$HITId == hit & tab.res$AssignmentStatus == "Rejected")
    tab.res <- tab.res[-which.remove,]
  }

  ## Total number of jobs completed
  njobs <- nrow(tab.res)

  ## Get appropriate cols of output for input/output parsing 
  header <- colnames(tab.res)
  pos.qid <- grep(pattern="Input.q[0-9]?id",x=header)
  pos.qans <- grep(pattern="Answer.q[0-9]{1}$",x=header)
  ## Make matrices of qids (what type of model?) and the answers
  ## Each row cooresponds to one HIT
  tab.qid.raw <- as.vector(t(as.matrix(tab.res[,pos.qid])))
  tab.qid.check <- matrix(tab.qid.raw,ncol=6,byrow=TRUE)
  ## Get the model types of out the question ids to tabulate outcomes
  tab.qid <- matrix(sapply(tab.qid.raw,function(x){strsplit(x,split="_q")[[1]][1]}),
                    nrow=njobs,byrow=TRUE)
  ## Isolate the answer columns
  tab.qans <- tab.res[,pos.qans]
  
  ## Load in answer key
  infile.key <- paste0(output.dir,"wi_t",t,"_r",r,"_ans.csv")
  tab.key <- read.csv(infile.key,header=FALSE)
  tab.key.check <- matrix(tab.key[,1],ncol=6,byrow=TRUE)
  output.check.mat <- cbind(tab.qid.check[,1],tab.key.check[,1])
  # Check to make sure lined up responses and answers correctly
  if(!all(output.check.mat[,1]==output.check.mat[,2])){
    stop("Response question ids do not match those in answer key")}
  key.mat <- matrix(tab.key[,2],nrow=njobs,byrow=TRUE)
  ## print(dim(tab.qans))
  ## print(dim(key.mat))
  ## For every question, get logical value indicating if response and answer match
  score.mat <- tab.qans-key.mat == 0

  ## Get the unique model names
  names.order <- tab.qid[1,][order(tab.qid[1,])]
  unique.names.order <- unique(names.order)
  ## Get the reponses for each HIT in the same model order
  model.score <- sapply(1:njobs,function(x){score.mat[x,][order(tab.qid[x,])]})
  rownames(model.score) <- names.order
  worker.res <- t(apply(model.score,2,res.agg,nrep=r))
  colnames(worker.res) <- unique.names.order
  agg.res <- apply(worker.res,2,sum,na.rm=TRUE)
  tot.comp.res <-  r*apply(worker.res,2,function(col){length(which(!is.na(col)))})
  prop.agg.res <- agg.res/tot.comp.res
  binom.ci.mat <- sapply(1:nmodels,function(pos){
    binom.test(x=agg.res[pos],n=tot.comp.res[pos])$conf.int})
  colnames(binom.ci.mat) <- names(agg.res)
  rownames(binom.ci.mat) <- c("lower","upper")
  binom.ci.list[[as.character(ntopics)]] <- binom.ci.mat
  
  n.trial.mat[as.character(ntopics),] <- tot.comp.res
  prop.success.mat[as.character(ntopics),] <- prop.agg.res
  if(ntopics==ntopics.use[1]){
    colnames(n.trial.mat) <- colnames(prop.success.mat) <- names(tot.comp.res)
  }
}

## n.trial.mat
## prop.success.mat
## binom.ci.list
binom.ci.vec <- unlist(binom.ci.list)
models <- colnames(n.trial.mat)
model.lty <- c(1:nmodels)
names(model.lty) <- models
model.pch <- c(1,2,4)
names(model.pch) <- models
pretty.model.names <- sapply(models,function(string){
  paste(strsplit(string,"_")[[1]][c(1,3)],collapse="_")})

file.pdf <- paste0(output.dir,"wi_res.pdf")
pdf(file.pdf,width=10,height=6)
for(j in 1:nmodels){
  model <- models[j]
  lower.ci.vec <- sapply(binom.ci.list,function(mat){mat[1,j]})
  upper.ci.vec <- sapply(binom.ci.list,function(mat){mat[2,j]})
  if(j == 1){
    plot(ntopics.use,prop.success.mat[,model],type="b",
         ylim=c(min(binom.ci.vec),max(binom.ci.vec)),
         lty=model.lty[model],pch=model.pch[model],
         ylab="Probability of detecting intruder",
         xlab="Number of topics",xaxt="n")
    axis(side=1, at=ntopics.use)
  } else {
    lines(ntopics.use,prop.success.mat[,model],type="b",lty=model.lty[model],
          pch=model.pch[model])
  }
  # Plot error bars
  arrows(x0=ntopics.use,x1=ntopics.use,y0=lower.ci.vec,y1=upper.ci.vec,
         angle=90,code=3,length=0.1)
}
legend(x="topleft",legend=pretty.model.names,lty=model.lty,pch=model.pch)
dev.off()

## print(worker.res)
## print(agg.res)
## print(prop.agg.res)

## worker.analysis.raw <- cbind(tab.res$WorkerId,apply(worker.res,1,sum,na.rm=TRUE))
## worker.analysis <- do.call(rbind,tapply(worker.analysis.raw[,2],worker.analysis.raw[,1],function(x){c(mean(x)/6,length(x))}))
## colnames(worker.analysis) <- c("Success rate","Njobs")
## print(worker.analysis)
