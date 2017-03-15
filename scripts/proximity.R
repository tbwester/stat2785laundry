source("functions.R")
dorms <- c("burcou", "hithal", "inthou", "maxpalcen", "maxpalwes")
p_list <- c()
p_list2 <- c()
p_list3 <- c()
dist_list <- c()
dist_list2 <- c()
dist_list3 <- c()

find_pval <- function(i, j, k, points) {
  fitpts_trim <- points[which((points$y == i | points$y == j) & points[,i+4] == 0 & points[,j+4] == 0 ),]
  
  ## P(i & i,j free & k free)
  a <- length(fitpts_trim[which(fitpts_trim$y == i & fitpts_trim[,k+4] == 1),1])
  b <- length(fitpts_trim[which(fitpts_trim$y == j & fitpts_trim[,k+4] == 1),1])
  c <- length(fitpts_trim[which(fitpts_trim$y == i & fitpts_trim[,k+4] == 0),1])
  d <- length(fitpts_trim[which(fitpts_trim$y == j & fitpts_trim[,k+4] == 0),1])
  #print(a + b + c + d)
  ## Difference of proportions
  p1 <- a / (a + b)
  p2 <- c / (c + d)
  std <- sqrt(p1*(1-p1)/(a+b) + p2*(1-p2)/(c+d))
  z <- (p1 - p2) / std
  return(pnorm(z))
}

for (dorm in dorms) {
  print(dorm)
  data <- setupdata(dorm)
  
  ## total number of washers & dryers
  wash_list <- sort(unique(data$number[which(data$type=='w')]))
  dry_list <- sort(unique(data$number[which(data$type=='d')]))
  n_wash <- length(wash_list)
  n_dry <- length(dry_list)
  mac_list <- sort(c(wash_list, dry_list))
  
  #data <- data[100:length(data[,1]),] # cut the first 100 entries -- probably too low-desnity
  inds <- c() # indicator variables for each machine in-use
  for (i in 1:(length(data[,1])-1)) {
      data$y[i] <- 0
      for (it in (i+1):length(data[,1])) {
          if (data$type[it] == data$type[i]) {
              dt <- as.numeric(difftime(data$start_time[it],data$start_time[i]), units="mins")
              ## Skip events with no machines of the same type starting over 10 mins later
              if (dt > 15 || dt < 5) {
                  break
              }
              ## Once a valid machine in found, use it and break
              data$y[i] <- data$number[it]
              
              ## Randomize the result (testing only)
              #if (data$type[i] == 'w') {
              #  data$y[i] <- sample(wash_list,1,prob=washfreq)
              #}
              #else {
              #  data$y[i] <- sample(dry_list,1,prob=dryfreq)
              #}
              break
          }
      }
      ## Set up indicator variables
      inds <- rbind(inds, ind_func(data$inuse_list[i]))
      inds[i, data$number[i]] = 1 # make sure current machine is included in list of in-use machines
      
      if (data$type[i] == "w") {
          data$x1[i] <- data$inuse_count[i]/n_wash
      }
      else {
          data$x1[i] <- data$inuse_count[i]/n_dry
      }
      
      data$x2[i] <- as.numeric(data$type[i] == "d")
      data$x3[i] <- floor(timeofday(data$start_time[i]))
      
  }
  
  data_trim <- data[which(data$y != 0),]
  inds_trim <- inds[which(data$y != 0),]
  
  # create data frame with just Y and Xs
  fitpts = as.data.frame(cbind(data_trim$y, data_trim$x1, data_trim$x2, data_trim$x3, inds_trim))
  colnames(fitpts) <- c("y", "x1", "x2", "x3", as.character(1:length(mac_list)))
  
  ## First direction: Fix k as the first washer, move away
  for (pair in 1:(n_wash - 2)) {
    i <- pair + wash_list[1]
    j <- pair + wash_list[1] + 1
    k <- wash_list[1]
    
    pval <- find_pval(i, j, k, fitpts)
    
    p_list <- append(p_list, pval)
    dist_list <- append(dist_list, pair)
  }
  
  ## Second direction: Fix k as the last washer, move away
  for (pair in 1:(n_wash - 2)) {
    k <- wash_list[length(wash_list)]
    i <- k - pair
    j <- k - pair - 1
    
    pval <- find_pval(i, j, k, fitpts)
    
    p_list <- append(p_list, pval)
    dist_list <- append(dist_list, pair)
  }
  
  ## Third try: Fix k and i
  for (pair in 1:(n_wash - 2)) {
    k <- wash_list[1]
    i <- wash_list[1] + 1
    j <- wash_list[1] + 1 + pair
    
    pval <- find_pval(i, j, k, fitpts)
    
    p_list3 <- append(p_list3, pval)
    dist_list3 <- append(dist_list3, pair)
  }
}
