## Function to pull individual model scores from results
res.agg <- function(vec,nrep){
  n <- length(vec)
  nrow.mat <- n/nrep
  out <- apply(matrix(vec,nrow=nrow.mat,byrow=TRUE),1,sum)
  return(out)
}

# Inputs
t <- 50
r <- 2

## Load in file results file
main.dir <- "/home/jbischof/Project_output/word_rate_model/"
input.dir <- paste0(main.dir,"aturk_input/")
output.dir <- paste0(main.dir,"aturk_output/")
infile.res <- paste0(output.dir,"wi_t",t,"_r",r,"_results.csv")
tab.res <- read.csv(infile.res,header=TRUE)
# Only keep accepted assignments if there are duplicates
hit.dups <- tab.res$HITId[duplicated(tab.res$HITId)]
for (hit in hit.dups){
  which.remove <- which(tab.res$HITId == hit & tab.res$AssignmentStatus == "Rejected")
  tab.res <- tab.res[-which.remove,]
}
njobs <- nrow(tab.res)
nrep <- 2
header <- colnames(tab.res)
pos.qid <- grep(pattern="Input.q[0-9]?id",x=header)
pos.qans <- grep(pattern="Answer.q[0-9]{1}$",x=header)

tab.qid.raw <- as.vector(t(as.matrix(tab.res[,pos.qid])))
tab.qid <- matrix(sapply(tab.qid.raw,function(x){strsplit(x,split="_q")[[1]][1]}),
                  nrow=njobs,byrow=TRUE)
tab.qans <- tab.res[,pos.qans]

## Load in answer key
infile.key <- paste0(output.dir,"wi_t",t,"_r",r,"_ans.csv")
tab.key <- read.csv(infile.key,header=FALSE)
key.mat <- matrix(tab.key[,2],nrow=njobs,byrow=TRUE)
score.mat <- tab.qans-key.mat == 0

names.order <- tab.qid[1,][order(tab.qid[1,])]
unique.names.order <- unique(names.order)
model.score <- sapply(1:njobs,function(x){score.mat[x,][order(tab.qid[x,])]})
rownames(model.score) <- names.order
worker.res <- t(apply(model.score,2,res.agg,nrep=2))
colnames(worker.res) <- unique.names.order
agg.res <- apply(worker.res,2,sum,na.rm=TRUE)
prop.agg.res <- agg.res/(njobs*nrep)

print(worker.res)
print(agg.res)
print(prop.agg.res)

worker.analysis.raw <- cbind(tab.res$WorkerId,apply(worker.res,1,sum,na.rm=TRUE))
worker.analysis <- do.call(rbind,tapply(worker.analysis.raw[,2],worker.analysis.raw[,1],function(x){c(mean(x)/6,length(x))}))
colnames(worker.analysis) <- c("Success rate","Njobs")
print(worker.analysis)
