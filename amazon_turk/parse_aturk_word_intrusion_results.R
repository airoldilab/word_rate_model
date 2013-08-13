## Function to pull individual model scores from results
res.agg <- function(vec,nrep){
  n <- length(vec)
  nrow.mat <- n/nrep
  out <- apply(matrix(vec,nrow=nrow.mat,byrow=TRUE),1,sum)
  return(out)
}


## Load in file results file
infile.res <- "Batch_1236481_batch_results.csv"
tab.res <- read.csv(infile.res,header=TRUE)
njobs <- nrow(tab.res)
header <- colnames(tab.res)
pos.qid <- grep(pattern="Input.q[0-9]?id",x=header)
pos.qans <- grep(pattern="Answer.q[0-9]{1}$",x=header)

tab.qid.raw <- as.vector(t(as.matrix(tab.res[,pos.qid])))
tab.qid <- matrix(sapply(tab.qid.raw,function(x){strsplit(x,split="_q")[[1]][1]}),
                  nrow=njobs,byrow=TRUE)
tab.qans <- tab.res[,pos.qans]

## Load in answer key
infile.key <- "aturk_word_intrusion_t100_j10_r2_ans.csv"
tab.key <- read.csv(infile.key,header=FALSE)
key.mat <- matrix(tab.key[,2],nrow=njobs,byrow=TRUE)
score.mat <- tab.qans-key.mat == 0

names.order <- tab.qid[1,][order(tab.qid[1,])]
unique.names.order <- unique(names.order)
model.score <- sapply(1:njobs,function(x){score.mat[x,][order(tab.qid[x,])]})
rownames(model.score) <- names.order
worker.res <- t(apply(model.score,2,res.agg,nrep=2))
colnames(worker.res) <- unique.names.order

#cbind(tab.res$WorkerId,final.res)
