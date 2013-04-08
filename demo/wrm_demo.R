src.dir <- "../R/"
out.dir <- "/home/jbischof/Project_output/word_rate_model/"
source(paste0(src.dir,"wrm_fit.R"))
data.file <- paste0(out.dir,"ap_ragarray.txt")
doc.length.file <- paste0(out.dir,"ap_doclengths.txt")
file.out <- paste0(out.dir,"wrm_out.RData")

ntopics <- 10
d.use <- 2246
wc.all <- read.table(file=data.file,sep="\t",
                     col.names=c("d","f","count"))
wc <- subset(wc.all,d<=d.use)
doc.length.vec.all <- read.table(file=doc.length.file,sep="\t",
                              row.names=1)[,1]
# Normalize doc length by length of average document
doc.length.vec <- doc.length.vec.all[1:d.use]/mean(doc.length.vec.all)

# Initialize parallel computation
init.parallel(cores=4)

source(paste0(src.dir,"wrm_fit.R"))
#debug(wrm.fit)
wrm.out <- wrm.fit(
             # data.frame with columns d,f,count
             wc=wc,
             # Vector of document lengths
             doc.length.vec=doc.length.vec,
             # Number of topics
             ntopics=ntopics,
             # Iterations
             iter=10,burnin=0,
             # Parallel computation?
             parallel=FALSE,
             verbose=TRUE)
save(wrm.out,file=file.out)
round(head(wrm.out$lambda.mat),5)
round(head(wrm.out$theta.mat),5)
