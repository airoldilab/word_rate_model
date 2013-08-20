# Inputs
output.dir <- "/home/jbischof/Project_output/word_rate_model/aturk_output/"
ntopics.use <- c(10,25,50,100)
r <- 1
nmodels <- 3
relpref.success.mat <- matrix(NA,ncol=nmodels+1,nrow=length(ntopics.use))
rownames(relpref.success.mat) <- ntopics.use
n.relpref.mat <- relpref.success.mat

topic.rate.mat <- relpref.success.mat[,1:3]
n.rate.mat <- topic.rate.mat

binom.ci.list <- list()
rate.ci.list <- list()


for(ntopics in ntopics.use){
  t <- ntopics
  main.dir <- "/home/jbischof/Project_output/word_rate_model/"
  input.dir <- paste0(main.dir,"aturk_input/")
  output.dir <- paste0(main.dir,"aturk_output/")
  infile.res <- paste0(output.dir,"tc_t",t,"_r",r,"_results.csv")
  tab.res <- read.csv(infile.res,header=TRUE)
  njobs <- nrow(tab.res)
  nrep <- r
  header <- colnames(tab.res)
  pos.qid <- grep(pattern="Input.topic[0-9]{2}id",x=header)
  pos.qans <- grep(pattern="Answer.topic[0-9]{2}$",x=header)
  pos.relans <- grep(pattern="Answer.relpref",x=header)
  
  ##print(header[pos.qid])
  ##print(header[pos.qans])
  ##print(header[pos.relans])
  
  qid.mat <- tab.res[,pos.qid]
  qid.mat.relpref <- cbind(qid.mat,"No preference")
  score.mat <- tab.res[,pos.qans]
  relans <- tab.res[,pos.relans]
  relans[relans == 999] <- 3
  
  ## Get absolute score result
  names.order <- as.vector(as.matrix(qid.mat[1,][order(qid.mat[1,])]))
  worker.res <- t(sapply(1:njobs,function(x){
    as.numeric(score.mat[x,][order(qid.mat[x,])])}))
  colnames(worker.res) <- names.order
  agg.res <- apply(worker.res,2,sum,na.rm=TRUE)
  mean.agg.res <- apply(worker.res,2,mean,na.rm=TRUE)
  ## sd.agg.res <- apply(worker.res,2,sd,na.rm=TRUE)
  n.agg.res <- apply(worker.res,2,function(col){length(which(!is.na(col)))})
  rate.ci.mat <- sapply(1:nmodels,function(pos){
    t.test(x=worker.res[,pos])$conf.int})
  colnames(rate.ci.mat) <- names(agg.res)
  rownames(rate.ci.mat) <- c("lower","upper")
  rate.ci.list[[as.character(ntopics)]] <- rate.ci.mat
  
  n.rate.mat[as.character(ntopics),] <- n.agg.res
  topic.rate.mat[as.character(ntopics),] <- mean.agg.res
  if(ntopics==ntopics.use[1]){
    colnames(n.rate.mat) <- colnames(topic.rate.mat) <-
      names(mean.agg.res)
  }
  
  ## Get relative pref result
  count.nona.relpref <- length(which(!is.na(relans)))
  relans.model <- sapply(1:njobs,function(pos){
    if(is.na(relans[pos])){return(NA)
                         } else {
                           as.character(qid.mat.relpref[pos,(relans[pos] + 1)])}
  })
  relpref.res <- table(relans.model)
  prop.relpref.res <- table(relans.model)/count.nona.relpref
  
  binom.ci.mat <- sapply(1:(nmodels+1),function(pos){
    binom.test(x=relpref.res[pos],n=count.nona.relpref)$conf.int})
  colnames(binom.ci.mat) <- names(relpref.res)
  rownames(binom.ci.mat) <- c("lower","upper")
  binom.ci.list[[as.character(ntopics)]] <- binom.ci.mat

  
  n.relpref.mat[as.character(ntopics),] <- count.nona.relpref
  relpref.success.mat[as.character(ntopics),] <- prop.relpref.res
  if(ntopics==ntopics.use[1]){
    colnames(n.relpref.mat) <- colnames(relpref.success.mat) <-
      names(prop.relpref.res)
  }

  ## print(worker.res)
  ## print(agg.res)
  ## print(mean.agg.res)
  ## print(relpref.res)
  ## print(prop.relpref.res)
  
  ## table((table(tab.res$HITId)))
  
}

n.relpref.mat
relpref.success.mat
binom.ci.list



binom.ci.vec <- unlist(binom.ci.list)
models <- colnames(n.relpref.mat)[1:nmodels]
model.lty <- c(1:nmodels)
names(model.lty) <- models
model.pch <- c(1,2,4)
names(model.pch) <- models
pretty.model.names <- sapply(models,function(string){
  paste(strsplit(string,"_")[[1]][c(1,3)],collapse="_")})


file.pdf <- paste0(output.dir,"tc_relpref_res.pdf")
pdf(file.pdf,width=10,height=6)
for(j in 1:nmodels){
  model <- models[j]
  lower.ci.vec <- sapply(binom.ci.list,function(mat){mat[1,j]})
  upper.ci.vec <- sapply(binom.ci.list,function(mat){mat[2,j]})
  if(j == 1){
    plot(ntopics.use,relpref.success.mat[,model],type="b",
         ylim=c(min(binom.ci.vec),max(binom.ci.vec)),
         lty=model.lty[model],pch=model.pch[model],
         ylab="Probability of preferring summary",
         xlab="Number of topics",xaxt="n")
    axis(side=1, at=ntopics.use)
  } else {
    lines(ntopics.use,relpref.success.mat[,model],type="b",lty=model.lty[model],
          pch=model.pch[model])
  }
  # Plot error bars
  arrows(x0=ntopics.use,x1=ntopics.use,y0=lower.ci.vec,y1=upper.ci.vec,
         angle=90,code=3,length=0.1)
}
legend(x="topleft",legend=pretty.model.names,lty=model.lty,pch=model.pch)
dev.off()


n.rate.mat
topic.rate.mat

rate.ci.vec <- unlist(rate.ci.list)
models <- colnames(n.rate.mat)
model.lty <- c(1:nmodels)
names(model.lty) <- models
model.pch <- c(1,2,4)
names(model.pch) <- models
pretty.model.names <- sapply(models,function(string){
  paste(strsplit(string,"_")[[1]][c(1,3)],collapse="_")})


file.pdf <- paste0(output.dir,"tc_rate_res.pdf")
pdf(file.pdf,width=10,height=6)
for(j in 1:nmodels){
  model <- models[j]
  lower.ci.vec <- sapply(rate.ci.list,function(mat){mat[1,j]})
  upper.ci.vec <- sapply(rate.ci.list,function(mat){mat[2,j]})
  if(j == 1){
    plot(ntopics.use,topic.rate.mat[,model],type="b",
         ylim=c(min(rate.ci.vec),max(rate.ci.vec)),
         lty=model.lty[model],pch=model.pch[model],
         ylab="Average rating for summary on 1-3 scale",
         xlab="Number of topics",xaxt="n")
    axis(side=1, at=ntopics.use)
  } else {
    lines(ntopics.use,topic.rate.mat[,model],type="b",lty=model.lty[model],
          pch=model.pch[model])
  }
  # Plot error bars
  arrows(x0=ntopics.use,x1=ntopics.use,y0=lower.ci.vec,y1=upper.ci.vec,
         angle=90,code=3,length=0.1)
}
legend(x="topleft",legend=pretty.model.names,lty=model.lty,pch=model.pch)
dev.off()
