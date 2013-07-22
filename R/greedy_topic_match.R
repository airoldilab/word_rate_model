# Function to greedily match one set of word-topic loadings to another
# This is one-to-one matching without rotation

greedy.topic.match <- function(# New set of topics to be matched with reference set (DxK)
                               topic.new,
                               # Set of reference topics (DxK.ref where K.ref >= K)
                               topic.ref,
                               # Row offsets for either matrix
                               row.new.mean=NULL,
                               row.ref.mean=NULL){

  #topic.mat <- matrix(rnorm(50*5),50,5)
  #topic.mat.ref <- matrix(rnorm(50*10),50,10)

  # Get number of topics in each dictionary
  K <- ncol(topic.new)
  K.ref <- ncol(topic.ref)

  # Get rid of -Inf values
  topic.new[topic.new==-Inf] <- -750
  topic.ref[topic.ref==-Inf] <- -750

  if(K > K.ref){stop("Number of new topics greater than number of reference topics.")}

  # Demean rows of either matrix if requested
  if(!is.null(row.new.mean)){topic.new <- sweep(topic.new,1,row.new.mean,"-",check.margin=TRUE)}
  if(!is.null(row.ref.mean)){topic.ref <- sweep(topic.ref,1,row.ref.mean,"-",check.margin=TRUE)}
  
  # Make colnames for dictionaries if don't already have them
  if(is.null(colnames(topic.new))){colnames(topic.new) <- 1:K}
  if(is.null(colnames(topic.ref))){colnames(topic.ref) <- 1:K.ref}

  # Get correlation matrix between two topic dictionaries
  cor.topics.mat <- cor(topic.new,topic.ref)
  dimnames(cor.topics.mat) <- list(colnames(topic.new),colnames(topic.ref))

  # Output matrix of matches
  topic.match <- matrix(NA,nrow=K,ncol=2,dimnames=list(1:K,c("new_topic","ref_topic")))

  for(k in 1:K){
    best.match <- which(cor.topics.mat == max(cor.topics.mat),arr.ind=TRUE)[1,]
    topic.match[k,] <- c(rownames(cor.topics.mat)[best.match[1]],colnames(cor.topics.mat)[best.match[2]])
    if(k < K){
      cor.topics.mat <- cor.topics.mat[-best.match[1],-best.match[2],drop=FALSE]
    }
  }

  # Order matrix of matches in terms of reference topic
  ## order.ref <- sapply(colnames(topic.ref),
  ##                     function(x){which(topic.match[,"ref_topic"]==x)})
  ## topic.match <- topic.match[,c(2,1)]
  

  return(topic.match)
  
}
