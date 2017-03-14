fNames = c("hithal", "maxpalwes",
           "burcou", "inthou", "rengrarescom",
           "camnor", "maxpalcen", "snehal", 
           "camnornor", "maxpaleas")

timeofday <- function(date) {
  hrs=format(as.POSIXct(date, format="%Y-%m-%d %H:%M:%S"), format="%H")
  min=format(as.POSIXct(date, format="%Y-%m-%d %H:%M:%S"), format="%M")
  sec=format(as.POSIXct(date, format="%Y-%m-%d %H:%M:%S"), format="%S")
  
  hrs <-as.numeric(hrs)
  min <-as.numeric(min)
  sec<-as.numeric(sec)
  
  return(hrs + (min / 60) + (sec / 3600))
}

setBeggining = function(w_times, offset = 5){
  w_times_o = w_times - offset
  w_times_o[which(w_times_o < 0)] = w_times_o[which(w_times_o < 0)] + 24
  return(w_times_o)
}

get_dryer_times = function(data){
  
  all_times = list()
  dry_list <- unique(data$number[which(data$type=='d')])
  for(d in dry_list){
    #title = paste("dryer", w, ",", "hitchcock")
    w_times_day = data$start_time[which(data$number == d)]
    w_times = timeofday(w_times_day)
    all_times[[paste("dryer", d, sep = "_")]] = w_times
  }
  return(all_times)
}

get_data = function(fileName){
  data <- read.csv(fileName, header = FALSE)
  names(data) <- c("start_time", "end_time", "extend_time", "idle_time",
                   "number", "type", "dorm")
  data$start_time = as.POSIXct(data$start_time)
  o = order(data$start_time)
  data = data[o, ]
  return(data)
}

get_coin = function(fileCoin){
  coin_list = readLines(fileCoin)
  coinsN = list() 
  for (i in 1:length(coin_list )) {
    thelist <- strsplit(coin_list[i], ",")[[1]]
    thelist <- unique(thelist)
    thelistN = as.numeric(thelist)
    if(length( thelistN) ){
      coinsN[[i]] = thelistN 
    }
    else{
      coinsN[[i]] = 0 
    }
  }
  return(coinsN)
}

#function that returns start times of each washer machine on a 24 hour scale (list), and 
#another list of the coinciedences for each laundry machine 
get_washer_times = function(data){
  all_times = list()
  wash_list <- sort( unique(data$number[which(data$type=='w')]) ) 
  for(w in wash_list){
    print(w)
    ind_w = which(data$number == w)
    #title = paste("washer", w, ",", "hitchcock")
    w_times_day = data$start_time[ind_w]
    w_times = timeofday(w_times_day)
    
    all_times[[paste("washer", w, sep = "_")]] = w_times
  }
  return(all_times)
}

#function that returns list of the coinciedences for each laundry machine 
get_washer_coin = function(data, coin_list){
  all_coins = list() 
  wash_list <- sort( unique(data$number[which(data$type=='w')]) ) 
  for(w in wash_list){
    ind_w = which(data$number == w)
    #title = paste("washer", w, ",", "hitchcock")
    w_coin = coin_list[ind_w]
    all_coins[[paste("washer", w, "coin", sep = "_")]] = w_coin
  }
  return(all_coins)
}

#Returns the indices of times where i does not have j running 
get_free_choice = function(i, j, coins){
  coin_i = coins[[i]]
  indices = c() 
  for(k in 1:length(coin_i)){
    free = TRUE
    for(lbl in coin_i[[k]]){
      if(lbl == j){free = FALSE; break}
    }
    if(free){indices = append(indices, k)}
  }
  return(indices)
}


ks_machines = function(times, coins){
  oldw <- getOption("warn")
  options(warn = -1) 
  options(warn = oldw)
  
  n = length(times)
  pvals = matrix(rep(0, n^2), ncol = n)
  for(i in 1:n){
    for(j in i:n){
      x_ind = get_free_choice(i, j, coins)
      y_ind = get_free_choice(j, i, coins)
      ks_res = ks.test(x = times[[i]][x_ind], y = times[[j]][y_ind], 
                       alternative = "two.sided")
      pvals[i, j] = ks_res$p.value
      pvals[j, i] = ks_res$p.value
    }
  }
  colnames(pvals) = names(times)
  rownames(pvals) = names(times)
  
  return(pvals)
}

get_all_ks_tests = function(fNames){
  ks_results = list() 
  for(i in 1:length(fNames)){
    fileName = paste("../data/", fNames[i], ".csv", sep = "")
    data = get_data(fileName)
    
    fileCoin = paste("../data/coinc/", fNames[i], "_coin.txt", sep = "")

    n_machines = n_machines + length( unique(data$number) ) 
    
    coin_list = get_coin(fileCoin)
     
    w_times = get_washer_times(data)
    w_coins = get_washer_coin(data, coin_list)
    
    #d_times = get_dryer_times(data)
    
    w_ks_p = ks_machines(w_times, w_coins)
    #d_ks_p = ks_machines(d_times)
    
    ks_results[[paste(fNames[i], "Washers", sep = "_")]] = w_ks_p
    #ks_results[[paste(fNames[i], "Dryers", sep = "_")]] = d_ks_p
  } 
  return(ks_results)
}

get_n_test = function(ks_results){
  n = 0 
  for(i in 1:length(ks_results)){
    nt = ncol( ks_results[[i]])
    n = n + nt * (nt+1) / 2 
  }
  return(n)
}

flatten_pvals = function(pvals, dorm){
  names = c() 
  p = c() 
  for(i in 1:ncol(pvals)){
    for(j in i:ncol(pvals)){
      p = c(p, pvals[i, j])
      names = c(names, paste(dorm, rownames(pvals)[i], colnames(pvals)[j],
                             sep = "_") ) 
    }
  }
  p_m = as.matrix(p)
  rownames(p_m) = names
  return(p_m)
}

get_significant_pvals = function(ks_results, n_cmp, fNames, alpha = 0.05){
  signals = c() 
  for(i in 1:length(ks_results)){
    dorm = fNames[i]
    pvals = ks_results[[i]]
    p = flatten_pvals(pvals, dorm)
    p_bh = p.adjust(p[,1], "BH", n = n_cmp)
    signals = c( signals, rownames(p)[ which(p_bh < alpha) ] ) 
  }
  return(signals)
}

#ks_all_dorms = get_all_ks_tests(fNames)
#n_cmp = get_n_test(ks_all_dorms)
#signals = get_significant_pvals(ks_all_dorms, n_cmp, fNames, alpha = 0.05)
#signals
#Nothing is significant... LMAO 
#total = 233
#ks_all = ks_all_dorms

