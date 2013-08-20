library("plotrix")

## Function to pull individual model scores from results
res.agg <- function(vec,nrep){
  n <- length(vec)
  nrow.mat <- n/nrep
  out <- apply(matrix(vec,nrow=nrow.mat,byrow=TRUE),1,sum)
  return(out)
}

plot.interval <- function(ci.array,order.numeric=FALSE,cex.xaxis=1,
                          las.xaxis=0,
                          # Are the category names numeric?
                          cat.numeric=FALSE,
                          ...){
  if(order.numeric){ci.array <- ci.array[order(as.numeric(rownames(ci.array))),]}
  ncat <- nrow(ci.array)
  if(cat.numeric){
    x.lab.pos <- as.numeric(rownames(ci.array))
  } else {
    x.lab.pos <- 1:ncat
  }
  plotCI(x=ci.array[,"est"],ui=ci.array[,"upper"],gap=0,xlab="",
         li=ci.array[,"lower"],err="y",xaxt="n", ...)#xlim=c(0.5,ncat+0.5),...)
  axis(side=1, at=x.lab.pos, labels=rownames(ci.array), cex.axis=cex.xaxis,
       las=las.xaxis)
}

