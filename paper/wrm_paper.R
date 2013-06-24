src.dir <- "../R/"
source(paste0(src.dir,"wrm_fit.R"))
source(paste0(src.dir,"wrm_analyze.R"))
source(paste0(src.dir,"hparam_sample.R"))
library("Matrix")

## Get game from command line argument
c.args <- commandArgs(TRUE)
if(length(c.args)==0){
  ntopics <- 10
  burnin <- 10
  iter <- 10
  debug <- TRUE
  data.dir <- "/n/airoldifs2/lab/jbischof/word_rate_output/"
  debug.tag <- ifelse(debug,"_debug","")
  run.tag <- paste0("_k",ntopics,"_i",iter,debug.tag)
  out.dir <- paste0(data.dir,"run",run.tag,"/")
} else {
  ntopics <- as.numeric(c.args[1])
  iter <- as.numeric(c.args[2])
  burnin <- as.numeric(c.args[3])
  debug <- as.numeric(c.args[4])
  data.dir <- c.args[5]
  out.dir <- c.args[6]
}

## debug.tag <- ifelse(debug,"_debug","")
## run.tag <- paste0("_k",ntopics,"_i",iter,debug.tag)
## out.dir <- paste0(data.dir,"run",run.tag,"/")
## dir.create(out.dir, showWarnings = FALSE)

## Output filename
file.out <- paste0(out.dir,"wrm_out.RData")

## Load in data
data.file <- paste0(data.dir,"ap_ragarray.txt")
doc.length.file <- paste0(data.dir,"ap_doclengths.txt")
vocab.file <- paste0(data.dir,"vocab.txt")
wc.all <- read.table(file=data.file,sep="\t",
                     colClasses="integer",
                     col.names=c("d","f","count"))
doc.length.vec.all <- read.table(file=doc.length.file,sep="\t",
                              colClasses="integer",row.names=1)[,1]
vocab <- read.table(file=vocab.file,colClasses="character")[,1]
names(vocab) <- 1:length(vocab) - 1

## Number of documents to use (depends on if debug run)
if(debug){d.use <- ntopics*10} else {d.use <- "all"}
## Should only subset of data be used for fitting purposes?
if(d.use=="all"){
  wc <- wc.all
  ## Normalize doc length by length of average document
  doc.length.vec <- doc.length.vec.all/mean(doc.length.vec.all)
} else {
  wc <- subset(wc.all,d<=d.use)
  ## Normalize doc length by length of average document
  doc.length.vec <- doc.length.vec.all[1:d.use]/mean(doc.length.vec.all)
}

##source(paste0(src.dir,"wrm_fit.R"))
##debug(wrm.fit)
wrm.out <- wrm.fit(
             wc.obs=wc$count,
             d.index=wc$d,
             f.index=wc$f,
             ## Vector of document lengths
             doc.length.vec=doc.length.vec,
             ## Number of topics
             ntopics=ntopics,
             ## Should hyperparameters be inferred?
             hparam.draw=TRUE,
             ## Iterations (to save)
             iter=iter,
             ## Burnin iterations (added to iter)
             burnin=burnin,
             ## Number of words and docs to trace
             ndocs.trace=50,nwords.trace=50,
             ## Save output every 100 iters?
             file.out=file.out,
             verbose=TRUE,
             metro.correct=FALSE,
             save.burnin=debug)
save(wrm.out,file=file.out)

## Analyze model fit
##load(file.out)

## Find top-loading words in each topic
frex.sum <- get.top.words(wrm.out,n.get=20,vocab=vocab,type="frex",weight.freq=0.5)
freq.sum <- get.top.words(wrm.out,n.get=20,vocab=vocab,type="freq",weight.freq=0.5)

frex.file.out <- paste0(out.dir,"frex_ap_sum.txt")
freq.file.out <- paste0(out.dir,"freq_ap_sum.txt")
write.table(t(frex.sum),file=frex.file.out,sep=" ",quote=FALSE,
                        row.names=1:ntopics,col.names=FALSE)
write.table(t(freq.sum),file=freq.file.out,sep=" ",quote=FALSE,
                        row.names=1:ntopics,col.names=FALSE)


## Create a run-specific directory to contain trace plots
traceplot.dir <- paste0(out.dir,"trace_plots/")
dir.create(traceplot.dir, showWarnings=FALSE)

## Check that model converged
## Obs level latent variables
nitems.trace <- 15
for(i in 1:nitems.trace){
  file.wordtrace <- paste0(traceplot.dir,"word_trace",i,".pdf")
  file.doctrace <- paste0(traceplot.dir,"doc_trace",i,".pdf")
  file.exctrace <- paste0(traceplot.dir,"exc_trace",i,".pdf")
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
## Hyperparameters
for(hparam in c("alpha","beta","psi")){
  file.htrace <- paste0(traceplot.dir,hparam,"_trace.pdf")
  pdf(file.htrace,width=12,height=6)
  trace.hparam(wrm.out,type=hparam)
  dev.off()
}

## Do same for burnin draws
if(debug){
  ## Obs level latent variables
  nitems.trace <- 15
  for(i in 1:nitems.trace){
    file.wordtrace <- paste0(traceplot.dir,"word_trace_burnin",i,".pdf")
    file.doctrace <- paste0(traceplot.dir,"doc_trace_burnin",i,".pdf")
    file.exctrace <- paste0(traceplot.dir,"exc_trace_burinin",i,".pdf")
    pdf(file.wordtrace,width=10,height=6)
    trace.wrm(wrm.out,type="word",pos=i,burnin.draws=TRUE)
    dev.off()
    pdf(file.doctrace,width=10,height=6)
    trace.wrm(wrm.out,type="doc",pos=i,burnin.draws=TRUE)
  dev.off()
    pdf(file.exctrace,width=10,height=6)
    trace.wrm(wrm.out,type="exc",pos=i,burnin.draws=TRUE)
    dev.off()
  }
  ## Hyperparameters
  for(hparam in c("alpha","beta","psi")){
    file.htrace <- paste0(traceplot.dir,hparam,"_trace_burnin.pdf")
    pdf(file.htrace,width=12,height=6)
    trace.hparam(wrm.out,type=hparam,burnin.draws=TRUE)
    dev.off()
  }
}

## Create a run-specific directory to contain trace plots
frexplot.dir <- paste0(out.dir,"frex_plots/")
dir.create(frexplot.dir, showWarnings=FALSE)
##source(paste0(src.dir,"wrm_analyze.R"))
##debug(get.frex.plot)
get.frex.plot(wrm.out,vocab,
              plot.dir=frexplot.dir,
              lower.quant.cut.full=0.01,
              upper.quant.zoom=0.95,
              ## Plot parameters
              res.plot=200,
              size.inch=8)



