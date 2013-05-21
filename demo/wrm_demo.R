src.dir <- "../R/"
out.dir <- "/n/airoldifs2/lab/jbischof/word_rate_output/"
source(paste0(src.dir,"wrm_fit.R"))
source(paste0(src.dir,"wrm_analyze.R"))
data.file <- paste0(out.dir,"ap_ragarray.txt")
doc.length.file <- paste0(out.dir,"ap_doclengths.txt")
vocab.file <- paste0(out.dir,"vocab.txt")
file.out <- paste0(out.dir,"wrm_out.RData")
library("Matrix")

ntopics <- 10
#d.use <- 10
d.use <- "all"
#wc.all <- scan(file=data.file,sep="\t",
#               what=as.list(integer(3)))
wc.all <- read.table(file=data.file,sep="\t",
                     colClasses="integer",
                     col.names=c("d","f","count"))
doc.length.vec.all <- read.table(file=doc.length.file,sep="\t",
                              colClasses="integer",row.names=1)[,1]
vocab <- read.table(file=vocab.file,colClasses="character")[,1]
names(vocab) <- 1:length(vocab) - 1

# Should only subset of data be used for fitting purposes?
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
#debug(wrm.fit)
wrm.out <- wrm.fit(
             wc.obs=wc$count,
             d.index=wc$d,
             f.index=wc$f,
             # Vector of document lengths
             doc.length.vec=doc.length.vec,
             # Number of topics
             ntopics=ntopics,
             # Iterations
             iter=1500,burnin=500,
             ndocs.trace=50,nwords.trace=50,
             verbose=TRUE)
#save(wrm.out,file=file.out)
#round(head(wrm.out$ave.param.list$lambda.mat),5)
#round(head(wrm.out$ave.param.list$theta.mat),5)
#plot(wrm.out$final.param.list$lambda.mat[2,1,],type="b")

# Analyze model fit
load(file.out)

# Check that model converged
trace.wrm(wrm.out,type="word",pos=7)
trace.wrm(wrm.out,type="doc",pos=7)
trace.wrm(wrm.out,type="exc",pos=7)

# Find top-loading words in each topic
frex.sum <- get.top.words(wrm.out,n.get=20,vocab=vocab,type="frex",weight.freq=0.5)
freq.sum <- get.top.words(wrm.out,n.get=20,vocab=vocab,type="freq",weight.freq=0.5)

frex.file.out <- paste0(out.dir,"frex_ap_sum.txt")
freq.file.out <- paste0(out.dir,"freq_ap_sum.txt")
write.table(t(frex.sum),file=frex.file.out,sep=" ",quote=FALSE,
                        row.names=1:ntopics,col.names=FALSE)
write.table(t(freq.sum),file=freq.file.out,sep=" ",quote=FALSE,
                        row.names=1:ntopics,col.names=FALSE)

# Get FREX plots
plot.dir <- paste0(out.dir,"plots/")
source(paste0(src.dir,"wrm_analyze.R"))
debug(get.frex.plot)
get.frex.plot(wrm.out,vocab,
              plot.dir=plot.dir,
              lower.quant.cut.full=0.01,
              upper.quant.zoom=0.95,
              # Plot parameters
              res.plot=200,
              size.inch=8)



