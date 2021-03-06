## script to predict next machine to be used

require(rpart)
require(nnet)
library(doMC)
registerDoMC(detectCores() / 2)
library(parallel)

## merges coincidence files with data files
data <- as.data.frame(read.csv("../data/burcou.csv", header=FALSE))
names(data) <- c("start_time", "end_time", "extend_time", "idle_time", "number", "type", "dorm")

coin_list <- readLines("../data/coinc/burcou_coin.txt")

## total number of washers & dryers
wash_list <- unique(data$number[which(data$type=='w')])
dry_list <- unique(data$number[which(data$type=='d')])
n_wash <- length(wash_list)
n_dry <- length(dry_list)
mac_list <- sort(c(wash_list, dry_list))

coin_count <- c()
for (i in 1:length(coin_list)) {
  thelist <- strsplit(coin_list[i], ",")[[1]]
  thelist <- unique(thelist)
  coin_count <- append(coin_count, length(thelist))
}

## sort data
data <- data[order(data$start_time),]

## add index column
data$index <- 1:length(data[,1])

## merge
data$inuse_list <- coin_list
data$inuse_count <- coin_count

timeofday <- function(date) {
  hrs=format(as.POSIXct(date, format="%Y-%m-%d %H:%M:%S"), format="%H")
  min=format(as.POSIXct(date, format="%Y-%m-%d %H:%M:%S"), format="%M")
  sec=format(as.POSIXct(date, format="%Y-%m-%d %H:%M:%S"), format="%S")
  
  hrs <-as.numeric(hrs)
  min <-as.numeric(min)
  sec<-as.numeric(sec)
  
  return(hrs + (min / 60) + (sec / 3600))
}

ind_func <- function(string) {
  if (string == "") {
    return(numeric(length(mac_list)))
  }
  thelist <- strsplit(string, ",")[[1]]
  thelist <- as.numeric(sort(unique(thelist)))
  outvec <- c()
  for (i in 1:length(mac_list)) {
    if (is.element(mac_list[i], thelist)) {
      outvec <- append(outvec,1)
    }
    else {
      outvec <- append(outvec,0)
    }
  }
  return(outvec)
}

in_matrix <- function(str, m) {
  if (str == "") {
    return(0)
  }
  thelist <- strsplit(str, ",")[[1]]
  count <- 0
  for (i in 1:length(thelist)) {
    if (is.element(thelist[i],m)) {
      count <- count + 1
    }
  }
  return(count)
}

data <- data[100:length(data[,1]),] # cut the first 100 entries -- probably too low-desnity
inds <- c() # indicator variables for each machine in-use
for (i in 1:(length(data[,1])-1)) {
  
  data$y[i] <- 0
  for (it in (i+1):length(data[,1])) {
    if (data$type[it] == data$type[i]) {
      dt <- as.numeric(difftime(data$start_time[it],data$start_time[i]), units="mins")
      if (dt > 10) {
        break
      }
      data$y[i] <- data$number[it]
      break
    }
  }
  
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
fitpts = cbind(data_trim$y, data_trim$x1, data_trim$x2, data_trim$x3, inds_trim)
colnames(fitpts) <- c("y", "x1", "x2", "x3", as.character(1:length(mac_list)))


#INFERENCE

##Given Matrix of p values of multinom coefficeints, finds which ones are significant with BH 
##correction 
get_signals_multinom = function(pvals){
  p_bh = p.adjust(pvals, method = "BH")
  p_bh_m = matrix(p_bh, ncol = ncol(pvals) )
  colnames(p_bh_m) = colnames(pvals)  
  rownames(p_bh_m) = rownames(pvals)
  return(p_bh_m)
}

df = as.data.frame(fitpts)

multi_fit = multinom(as.factor(y) ~ ., data = df)
s_mfit = summary(multi_fit)
save(s_mfit, file = "BJ_multilogit.Rdata")
z = s_mfit$coefficients / s_mfit$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

p_bh = get_signals_multinom(p)
p_bh < 0.05

##correlated p values? 
mean( cor(p) > 0.8)



#PREDICTION PART 
hm_list <- c()
hm_list = foreach(i = 1:1000, combine = rbind) %dopar% {
  samp <- sample(length(data_trim[,1]),1000) ## get 1000 random points
  
  data_train <- as.data.frame(fitpts[samp,])
  data_test <- as.data.frame(fitpts[-samp,])
  
  #treemodel <- rpart(y ~ ., data=data_train, method="class",control=rpart.control(minsplit=50, cp=0.001))
  #plot(treemodel)
  #text(treemodel)
  
  multimodel = multinom(as.factor(y) ~ ., data = data_train)
  
  # print results for training set
  #results <- predict(treemodel, type="class")
  res_mlogit = predict(multimodel)
  
  # print results for test set
  #results_test <- predict(treemodel, newdata=data_test, type="class")
  res_test_mlogit = predict(multimodel, newdata=data_test)
  #mean(res_test_mlogit == data_test$y)
  
  successes <- c()
  for (i in 1:(n_wash + n_dry)) {
    perc <- sum(res_test_mlogit[which(data_test$y==i)]==i) / sum(data_test$y==i)
    successes <- append(successes, perc)
  }
  
  washfreq <- table(data_test$y[which(data_test$x2==0)])
  dryfreq <- table(data_test$y[which(data_test$x2==1)])
  
  washfreq <- washfreq / sum(washfreq)
  dryfreq <- dryfreq / sum(dryfreq)
  
  hm <- mean(successes / c(dryfreq, washfreq))
  return(hm)
}
hm_list_v = unlist(hm_list)
hist(hm_list_v, main = "Multinomial Logit", xlab = "Success % / Random %")
write.csv(as.data.frame(hm_list_v), "multinomial_hm.csv")
#rpart.plot(treemodel, extra=100)

