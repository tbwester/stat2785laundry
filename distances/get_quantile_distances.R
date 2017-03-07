get_quantile_lables = function(dist){
  quants = quantile(dist, probs = seq(0.25, 1, by = 0.25))
  quants
  labels = c()
  for(d in dist){
    for(j in 1:length(quants) ){
      q = quants[j]
      if(d <= q){
        labels = c(labels, j)
        break 
      }
    }
  }
  return(labels)
}


