library("lda")

ap.docs <- read.documents(filename = "ap.dat")
ap.vocab <- read.vocab(filename = "vocab.txt")

logit <- function(x){log(x) - log(1-x)}

result <- lda.collapsed.gibbs.sampler(ap.docs,
                                      K=10,  ## Num clusters
                                      ap.vocab,
                                      num.iterations=200,  ## Num iterations
                                      alpha=0.1,
                                      eta=0.1,
                                        #initial=result$assignments,
                                        #burnin=200,
                                        compute.log.likelihood=TRUE)
top.topic.words(result$topics,num.words=10)

## for(i in 1:1000){
##   result <- lda.collapsed.gibbs.sampler(ap.docs,
##                                         K=10,  ## Num clusters
##                                         ap.vocab,
##                                         num.iterations=200,  ## Num iterations
##                                         alpha=0.1,
##                                         eta=0.1,
##                                         #initial=result$assignments,
##                                         #burnin=200,
##                                         compute.log.likelihood=TRUE)
##   if(i==1){
##     topic.mat.t <- result$topics
##   } else { topic.mat.t <- topic.mat.t + result$topics
##          }
## }

plot(result$log.likelihoods[1,])
topic.mat <- t(result$topics)
##topic.mat <- t(topic.mat.t)
word.counts <- rowSums(topic.mat)
topic.mat.order <- topic.mat[rev(order(word.counts)),]
topic.prop.order <- apply(topic.mat.order,2,function(col){col/sum(col)}) + 0.00001
word.counts.order <- word.counts[rev(order(word.counts))]
topic.exc.order <- t(apply(topic.mat.order,1,function(row){row/sum(row)}))
topic.var.order <- apply(log(topic.prop.order),1,var)
max.topic.exc.order <- apply(topic.exc.order,1,max)
plot(topic.var.order)
plot(max.topic.exc.order)
plot(log(word.counts.order),logit(max.topic.exc.order))
plot(log(word.counts.order),topic.var.order)
smoothScatter(log(word.counts.order),max.topic.exc.order)
smoothScatter(log(word.counts.order),topic.var.order)
head(topic.exc.order)
tail(topic.exc.order)
