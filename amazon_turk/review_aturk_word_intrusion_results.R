## Load helper functions
source("parse_aturk_functions.R")

infile <- comandArgs(TRUE)
infile.root <- strsplit(infile,".")[[1]][1]
outfile <- paste0(infile.root,"_checked.csv")

tab.res <- read.csv(infile.res,header=TRUE)

## Total number of jobs completed
njobs <- nrow(tab.res)

## Get appropriate cols of output for input/output parsing 
header <- colnames(tab.res)
pos.qid <- grep(pattern="Input.q[0-9]?id",x=header)
pos.qans <- grep(pattern="Answer.q[0-9]{1}$",x=header)


## Isolate the answer columns
tab.qans <- tab.res[,pos.qans]

tab.res$Approve <- apply(tab.qans,function(row){
  ifelse(all(!is.na(row)),"x","")})
tab.res$Reject <- apply(tab.qans,function(row){
  ifelse(any(is.na(row)),"Did not answer all questions.","")})

comments <- tab.res$Answer.comment
print(na.rm(comments))

write.csv(tab.res)
                                               
