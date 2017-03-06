get_quantile_lables = function(dist){
  q = quantile(dist)
  labels = c()
  for(d in dist){
    if(d <= q[["25%"]]){ labels = c(labels, 1)}
    else if(d <= q[["50%"]]){ labels = c(labels, 2)}
    else if(d <= q[["75%"]]){ labels = c(labels, 3)}
    else {labels = c(labels, 4)}
  }
  return(labels)
}


