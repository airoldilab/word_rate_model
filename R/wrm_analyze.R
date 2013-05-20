# Function to get word loadings in each topic

# Load in logit function
logit <- function(x){log(x)-log(1-x)}

# Function to calculate frex score
frex.score <- function(freq.quant,exc.quant,weight.freq=0.5){
  score <- 1/(weight.freq/freq.quant + (1-weight.freq)/exc.quant)
  return(score)
}

get.word.loadings <- function(wrm.out,type="frex",weight.freq=0.5){

  # Retrieve relevant model output 
  mu.mat <- wrm.out$ave.param.list$mu.mat
  logit.phi.mat <- logit(wrm.out$ave.param.list$phi.mat)
  ntopics <- ncol(mu.mat)

  # Get cdf rankings for each word in each topic
  mu.quant.mat <- t(apply(mu.mat,2,function(col){ecdf(col)(col)}))
  if(type=="freq"){return(mu.quant.mat)}
  phi.quant.mat <- t(apply(logit.phi.mat,2,function(col){ecdf(col)(col)}))
  if(type=="exc"){return(phi.quant.mat)}

  # Calculate matrix of frex scores
  frex.mat <- t(sapply(1:ntopics,function(k,weight.freq){
    frex.score(mu.quant.mat[,k],phi.quant.mat[,k],weight.freq)}),
                weight.freq=weight.freq)
  
  return(frex.mat)
}

# Function to get the top items in any vector
get.top.items <- function(vec,vec.labels,n.get){
  vec.order <- rev(order(vec))
  pos.top <- vec.order[1:n.get]
  items.top <- vec.labels[pos.top]
  return(items.top)
}

# Function to find top-loading words in each topic
get.top.words <- function(wrm.out,n.get,vocab,type="frex",weight.freq=0.5){

  # Get word ids
  word.ids <- rownames(wrm.out$ave.param.list$mu.mat)
  
  # Get desired word scores
  score.mat <- get.word.loadings(wrm.out=wrm.out,type=type,
                                 weight.freq=weight.freq)

  # Get word ids for top loading items in each topic
  top.ids.mat <- t(apply(score.mat,2,get.top.items,
                         vec.labels=word.ids,n.get=n.get))

  # Get word strings for top ids
  top.words.mat <- t(apply(top.ids.mat,function(col){
    vocab[as.character(col)]}))

  return(top.words.mat)
}
