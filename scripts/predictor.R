## script to predict next machine to be used

require(rpart)

## merges coincidence files with data files
data <- as.data.frame(read.csv("../data/burcou.csv"))
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
data$inuse_list <- head(coin_list, -1)
data$inuse_count <- head(coin_count, -1)

## bj distance functions
#a=matrix(c(1, 3, 5, 7, 9, 0, 0, 0, 0, 0, 0,
#           2, 4, 6, 8, 10, 11, 12, 13, 14, 15, 16), ncol = 11, byrow = TRUE)
#b=matrix(c(17, 18), ncol=2, byrow = TRUE)

a=matrix(c(1, 3, 5, 7, 9, 
           2, 4, 6, 8, 10 ), ncol = 5, byrow = TRUE)
b=matrix(c(11, 12, 13, 14, 15, 16, 17, 18), ncol=8, byrow = TRUE)
#c=matrix(c(17,18), ncol=2, byrow = TRUE)

m_dist <- function(num1, num2, m){
    nu1=data.frame(which(m==num1, arr.ind = TRUE))
    nu2=data.frame(which(m==num2, arr.ind = TRUE))
    return(abs(nu1$row-nu2$row)+abs(nu1$col-nu2$col))
}

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

## Split data into training and test sets
data <- data[100:length(data[,1]),] # cut the first 100 entries -- probably too low-desnity
td <- c() # stores time deltas (not used in model)
inds <- c() # indicator variables for each machine in-use
for (i in 1:(length(data[,1])-1)) {
    
    data$y[i] <- 0
    for (it in (i+1):length(data[,1])) {
        dt <- as.numeric(difftime(data$start_time[it],data$start_time[i]), units="mins")
        td <- append(td, dt)
        if (dt > 10) {
            break
        }
        if (data$type[it] == data$type[i]) {
          data$y[i] <- data$number[it]
          break
        }
    }
    
    inds <- rbind(inds, ind_func(data$inuse_list[i]))
    
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

hm_list <- c()
for (runs in 1:10) {
  samp <- sample(length(data_trim[,1]),1000) ## get 1000 random points
  
  data_train <- as.data.frame(fitpts[samp,])
  data_test <- as.data.frame(fitpts[-samp,])
  
  treemodel <- rpart(y ~ ., data=data_train, method="class",control=rpart.control(minsplit=50, cp=0.001))
  #plot(treemodel)
  #text(treemodel)
  
  # print results for training set
  results <- predict(treemodel, type="class")
  #success <- results==data_train$y
  #sum(success[which(data_train$x2==0)]) / length(data_train[which(data_train$x2 == 0),1])
  #sum(success[which(data_train$x2==1)]) / length(data_train[which(data_train$x2 == 1),1])
  
  # print results for test set
  results_test <- predict(treemodel, newdata=data_test, type="class")
  
  successes <- c()
  for (i in 1:(n_wash + n_dry)) {
    print(sum(results_test[which(data_test$y==i)]==i))
    perc <- sum(results_test[which(data_test$y==i)]==i) / sum(data_test$y==i)
    successes <- append(successes, perc)
  }
  
  washfreq <- table(data_test$y[which(data_test$x2==0)])
  dryfreq <- table(data_test$y[which(data_test$x2==1)])
  
  washfreq <- washfreq / sum(washfreq)
  dryfreq <- dryfreq / sum(dryfreq)
  
  hm <- sum(successes - c(dryfreq, washfreq))
  hm_list <- append(hm_list, hm)
}


#success_test <- results_test==data_test$y
#sum(success_test[which(data_test$x2==0)]) / length(data_test[which(data_test$x2 == 0),1])
#sum(success_test[which(data_test$x2==1)]) / length(data_test[which(data_test$x2 == 1),1])

