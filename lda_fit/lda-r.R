library("lda")

ap.docs <- read.documents(filename = "ap.dat")
ap.vocab <- read.vocab(filename = "vocab.txt")
topic.smooth <- 0.1

logit <- function(x){log(x) - log(1-x)}

result <- lda.collapsed.gibbs.sampler(ap.docs,
                                      K=100,  ## Num clusters
                                      ap.vocab,
                                      num.iterations=300,  ## Num iterations
                                      alpha=0.1,
                                      eta=topic.smooth,
                                      ##initial=result$assignments,
                                      ##burnin=200,
                                      compute.log.likelihood=TRUE)
top.topic.words(result$topics,num.words=10)
plot(result$log.likelihoods[1,])

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


topic.mat.nosort <- t(result$topics)
##topic.mat <- t(topic.mat.t)
word.counts.nosort <- rowSums(topic.mat.nosort)
order.word.counts <- rev(order(word.counts.nosort))
topic.mat <- topic.mat.nosort[order.word.counts,]
word.counts <- word.counts.nosort[order.word.counts]
topic.prop <- apply(topic.mat + topic.smooth,2,function(col){col/sum(col)})
topic.exc <- t(apply(topic.prop,1,function(row){row/sum(row)}))
topic.var <- apply(log(topic.prop),1,var)
topic.ent <- apply(log(topic.prop),1,entropy.dist)
max.topic.exc <- apply(topic.exc,1,max)
plot(topic.var)
plot(max.topic.exc)
plot(jitter(log(word.counts),amount=0.1),jitter(max.topic.exc,amount=0.1),cex=0.5)
plot(jitter(log(word.counts),amount=0.1),jitter(topic.ent,amount=0.1),cex=0.5)
plot(jitter(log(word.counts),amount=0.1),jitter(topic.var,amount=0.1),cex=0.5)

smoothScatter(log(word.counts),max.topic.exc)
smoothScatter(log(word.counts),topic.var)
head(topic.exc)
tail(topic.exc)
