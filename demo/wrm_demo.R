src.dir <- "../R/"
out.dir <- "/home/jbischof/Project_output/word_rate_model/"
source(paste0(src.dir,"wrm_fit.R"))
data.file <- paste0(out.dir,"ap_ragarray.txt")
doc.length.file <- paste0(out.dir,"ap_doclengths.txt")
file.out <- paste0(out.dir,"wrm_out.RData")
library(Matrix)

ntopics <- 10
#d.use <- 2246
d.use <- "all"
#wc.all <- scan(file=data.file,sep="\t",
#               what=as.list(integer(3)))
wc.all <- read.table(file=data.file,sep="\t",
                     colClasses="integer",
                     col.names=c("d","f","count"))
doc.length.vec.all <- read.table(file=doc.length.file,sep="\t",
                              colClasses="integer",row.names=1)[,1]
if(d.use=="all"){
  wc <- wc.all
  # Normalize doc length by length of average document
  doc.length.vec <- doc.length.vec.all/mean(doc.length.vec.all)
} else {
  wc <- subset(wc.all,d<=d.use)
  # Normalize doc length by length of average document
  doc.length.vec <- doc.length.vec.all[1:d.use]/mean(doc.length.vec.all)
}

source(paste0(src.dir,"wrm_fit.R"))
debug(wrm.fit)
wrm.out <- wrm.fit(
             wc.obs=wc$count,
             d.index=wc$d,
             f.index=wc$f,
             # Vector of document lengths
             doc.length.vec=doc.length.vec,
             # Number of topics
             ntopics=ntopics,
             # Iterations
             iter=10,burnin=0,
             verbose=TRUE)
save(wrm.out,file=file.out)
round(head(wrm.out$lambda.mat),5)
round(head(wrm.out$theta.mat),5)
