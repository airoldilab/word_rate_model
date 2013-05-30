src.dir <- "../R/"
source(paste0(src.dir,"wrm_fit.R"))
source(paste0(src.dir,"wrm_analyze.R"))
input.dir <- "/n/airoldifs2/lab/jbischof/word_rate_output/"
data.file <- paste0(input.dir,"ap_ragarray.txt")
doc.length.file <- paste0(input.dir,"ap_doclengths.txt")
vocab.file <- paste0(input.dir,"vocab.txt")
library("Matrix")


# Get game from command line argument
c.args <- commandArgs(TRUE)
debug.mode <- length(c.args) == 0
ntopics <- as.numeric(c.args[1])
iter <- as.numeric(c.args[2])
debug <- as.logical(as.numeric(c.args[3]))
run.tag <- paste0("_k",ntopics,"_i",iter)
out.dir <- paste0(input.dir,"run",run.tag,"/")
dir.create(out.dir, showWarnings = FALSE)
file.out <- paste0(out.dir,"wrm_out.RData")


if(debug){
  d.use <- 50
  burnin <- 0
} else {
  d.use <- "all"
  burnin <- 1500
}
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

#source(paste0(src.dir,"wrm_fit.R"))
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
             iter=iter+burnin,burnin=burnin,
             ndocs.trace=50,nwords.trace=50,
             # Save output every 100 iters?
             file.out=file.out,
             verbose=TRUE)
save(wrm.out,file=file.out)

# Analyze model fit
#load(file.out)

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
# Create a run-specific directory to contain plots
plot.dir <- paste0(out.dir,"plots/")
dir.create(plot.dir, showWarnings = FALSE)

# Check that model converged
nitems.trace <- 15
for(i in 1:nitems.trace){
  file.wordtrace <- paste0(plot.dir,"word_trace",i,".pdf")
  file.doctrace <- paste0(plot.dir,"doc_trace",i,".pdf")
  file.exctrace <- paste0(plot.dir,"exc_trace",i,".pdf")
  pdf(file.wordtrace,width=10,height=6)
  trace.wrm(wrm.out,type="word",pos=i)
  dev.off()
  pdf(file.doctrace,width=10,height=6)
  trace.wrm(wrm.out,type="doc",pos=i)
  dev.off()
  pdf(file.exctrace,width=10,height=6)
  trace.wrm(wrm.out,type="exc",pos=i)
  dev.off()
}

#source(paste0(src.dir,"wrm_analyze.R"))
#debug(get.frex.plot)
get.frex.plot(wrm.out,vocab,
              plot.dir=plot.dir,
              lower.quant.cut.full=0.01,
              upper.quant.zoom=0.95,
              # Plot parameters
              res.plot=200,
              size.inch=8)



