## Load in file results file
t <- 100
r <- 1

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

print(header[pos.qid])
print(header[pos.qans])
print(header[pos.relans])

qid.mat <- tab.res[,pos.qid]
qid.mat.relpref <- cbind(qid.mat,"No preference")
score.mat <- tab.res[,pos.qans]
relans <- tab.res[,pos.relans]
relans[relans == 999] <- 3

# Get absolute score result
names.order <- as.vector(as.matrix(qid.mat[1,][order(qid.mat[1,])]))
worker.res <- t(sapply(1:njobs,function(x){as.numeric(score.mat[x,][order(qid.mat[x,])])}))
colnames(worker.res) <- names.order
agg.res <- colSums(worker.res)
mean.agg.res <- agg.res/(njobs*nrep)

# Get relative pref result
relans.model <- sapply(1:njobs,function(pos){
  if(is.na(relans[pos])){return(NA)
  } else {
    as.character(qid.mat.relpref[pos,(relans[pos] + 1)])}
})
relpref.res <- table(relans.model)
prop.relpref.res <- table(relans.model)/njobs

print(worker.res)
print(agg.res)
print(mean.agg.res)
print(relpref.res)
print(prop.relpref.res)

#cbind(tab.res$WorkerId,final.res)
