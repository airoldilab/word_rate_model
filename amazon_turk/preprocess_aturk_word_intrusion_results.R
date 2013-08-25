## Script to load in aturk results, put in proper order,
## and append to any existing results

#infile <- comandArgs(TRUE)
out.dir <- "~/Project_output/word_rate_model/aturk_output/"
infile <- "~/Downloads/Batch_1247451_batch_results.csv"
#infile.root <- strsplit(infile,".csv")[[1]][1]
#outfile <- paste0(infile.root,"_ordered.csv")
outfile <- paste0(out.dir,"wi_t10_r2_results.csv")

tab.res <- read.csv(infile,header=TRUE)

## Total number of jobs completed
njobs <- nrow(tab.res)

## Get appropriate cols of output for input/output parsing 
header <- colnames(tab.res)
pos.qid <- grep(pattern="Input.q[0-9]?id",x=header)
pos.qans <- grep(pattern="Answer.q[0-9]{1}$",x=header)
pos.q1 <- pos.qid[1]

## Isolate the first answer column
q1.id.str <- as.character(tab.res[,pos.q1])

## Put the rows in order
q1.ids <- as.numeric(sapply(q1.id.str,function(string){
  strsplit(string,split="_q")[[1]][2]}))
q1.order <- order(q1.ids)
##q1.ids[q1.order]

tab.res.order <- tab.res[q1.order,]

write.table(tab.res.order,file=outfile,row.names=FALSE,
            append=TRUE,col.names=FALSE,quote=TRUE,sep=",")
                                               
