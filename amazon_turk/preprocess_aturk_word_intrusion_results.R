## Script to load in aturk results, put in proper order,
## and append to any existing results

infile <- comandArgs(TRUE)
infile.root <- strsplit(infile,".")[[1]][1]
outfile <- paste0(infile.root,".csv")

tab.res <- read.csv(infile.res,header=TRUE)

## Total number of jobs completed
njobs <- nrow(tab.res)

## Get appropriate cols of output for input/output parsing 
header <- colnames(tab.res)
pos.qid <- grep(pattern="Input.q[0-9]?id",x=header)
pos.qans <- grep(pattern="Answer.q[0-9]{1}$",x=header)

## Isolate the answer columns
tab.qans <- tab.res[,pos.qans]

## Put the rows in order

write.csv(tab.res)
                                               
