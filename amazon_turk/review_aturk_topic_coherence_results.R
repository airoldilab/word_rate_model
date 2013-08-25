## Script to automatically approve or reject aturk assignments

infile <- comandArgs(TRUE)
infile.root <- strsplit(infile,".csv")[[1]][1]
outfile <- paste0(infile.root,"_checked.csv")

tab.res <- read.csv(infile,header=TRUE)

## Total number of jobs completed
njobs <- nrow(tab.res)

## Get appropriate cols of output for input/output parsing 
header <- colnames(tab.res)
pos.qans <- grep(pattern="Answer.topic[0-9]{2}$",x=header)
pos.relans <- grep(pattern="Answer.relpref",x=header)

## Isolate the answer columns
tab.qans <- tab.res[,c(pos.qans,pos.relans)]

tab.res$Approve <- apply(tab.qans,1,function(row){
  ifelse(all(!is.na(row)),"x","")})
tab.res$Reject <- apply(tab.qans,1,function(row){
  ifelse(any(is.na(row)),"Did not answer all questions.","")})

comments.raw <- as.character(tab.res$Answer.comment)
comments <- comments.raw[nchar(comments.raw) > 0]
print(as.matrix(comments))

write.csv(tab.res,file=outfile)
                                               
