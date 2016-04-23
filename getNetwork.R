
getNetwork <- function(criterium,I){
  
  LL = 0
  for(i in criterium){
    LL = LL + sum(as.integer(I[,1]==i))
  }
  links = matrix(0*(1:(LL*2)),nrow=LL)
  k = 1
  for(i in criterium){
    n = length(I[I[,1]==i,1])-1
    links[k:(k+n),1] = I[I[,1]==i,1]
    links[k:(k+n),2] = I[I[,1]==i,2]
    k = k+n+1
  }
  nodes = matrix(unique(as.vector(links)),ncol=1)
  
  return(list(nodes=nodes,links=links))
  
}
