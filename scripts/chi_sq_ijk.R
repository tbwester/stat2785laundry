source("get_distributions.R")
require(MASS)

#Same as get_free_choice, but coins is now just a list of the entire dorm coincidecnes 
get_free_choice_ALL = function(i, j, data, coins){
  indices = c() 
  for(k in 1:length(coins)){
    if(data$number[k] == i){
      if(! (j %in% coins[[k]]) ) {indices = append(indices, k)}
    }
  }
  return(indices)
}

#Returns a vector c(count_k, count_no_k), where count_k is the count  
get_counts_machine_k = function(coins, ind_i, m_k){
  count_k = 0 
  coins_i = coins[ind_i]
  for(j in 1:length(coins_i)){
    for(lbl in coins_i[[j]]){
      if(lbl == m_k){count_k = count_k + 1; break}
    }
  }
  return(c(count_k, length(ind_i) - count_k))
}

i = 1
j = 2
k = 3
ind_i = get_free_choice_ALL(i, j, data, coin_list)
ind_j = get_free_choice_ALL(j, i, data, coin_list)

c_i = get_counts_machine_k(coin_list, ind_i, k)
c_j = get_counts_machine_k(coin_list, ind_j, k)

counts_close = matrix(c(c_i, c_j), nrow = 2, byrow = TRUE)
colnames(counts_close) = c("k in use", "|k not in use")
rownames(counts_close) = c("chose machine i", "chose machine j")
counts_close

