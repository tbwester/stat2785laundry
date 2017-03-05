## script to predict next machine to be used

## merges coincidence files with data files
data <- as.data.frame(read.csv("../data/burcou.csv"))
names(data) <- c("start_time", "end_time", "extend_time", "idle_time", "number", "type", "dorm")

coin_list <- readLines("../data/coinc/burcou_coin.txt")

## total number of washers & dryers
n_wash <- length(unique(data$number[which(data$type=='w')]))
n_dry <- length(unique(data$number[which(data$type=='d')]))

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
samp <- sample(length(data[,1]),1000) ## get 1000 random points
data_train <- data[samp,]
data_test <- data[-samp,]

ytrain <- c() # Response: Next machine that gets used
x1train <- c() # Predictor 1: % in-use of cluster
x2train <- c() # Predictor 2: 0/1 washer/dryer
x3train <- c() # Predictor 3: Time of day 0=morning, 1=afternoon, 2=night
for (i in 1:length(data_train[,1])) {
    nexty <- -1
    counter <- data_train$index[i] + 1 #index of next machine to start
    while (nexty == -1) {
        entry <- data[which(data$index==counter),]
        if (entry$type != data_train$type[i]) {
            counter = counter + 1
            next
        }
        if (is.element(entry$number, a) && !is.element(data_train$number[i], a) ) {
            counter = counter + 1
            next
        }
        nexty = entry$index
    }
    ytrain <- append(ytrain, data[which(data$index==nexty),"number"])
    
    
    if (data_train$type[i] == "w") {
        x1train <- append(x1train, data_train$inuse_count[i]/n_wash)
    }
    else {
        x1train <- append(x1train, data_train$inuse_count[i]/n_dry)
    }
    
    td <- 0
    dt <- timeofday(data_train$start_time[i])
    if (dt > 6 && dt < 12) {
        td <- 0
    }
    else if (dt > 12 && dt < 18) {
        td <- 1
    }
    else if (dt > 18 && dt < 24) {
        td <- 2
    }
    else {
        td <- 3
    }
    x3train <- append(x3train, td)
    x2train <- append(x2train, as.numeric(data_train$type[i] == "d"))
}

treemodel <- rpart(ytrain ~ x1train + x2train + x3train, method="class",control=rpart.control(minsplit=50, cp=0.001))
plot(treemodel)
text(treemodel)

results <- predict(treemodel, type="class")
head(results)
head(ytrain)

ytest <- c() # Response: Next machine that gets used
x1test <- c() # Predictor 1: % in-use of cluster
x2test <- c() # Predictor 2: 0/1 washer/dryer
x3test <- c() # Predictor 3: Time of day 0=morning, 1=afternoon, 2=night
for (i in 1:length(data_test[,1])) {
    nexty <- -1
    counter <- data_test$index[i] + 1 #index of next machine to start
    invalid <- FALSE
    while (nexty == -1) {
        entry <- data[which(data$index==counter),]
        if (dim(entry)[1] == 0) {
            invalid <- TRUE
            break
        }
        if (entry$type != data_test$type[i]) {
            counter = counter + 1
            next
        }
        if (is.element(entry$number, a) && !is.element(data_test$number[i], a) ) {
            counter = counter + 1
            next
        }
        nexty = entry$index
    }
    if (invalid) {
        next
    }
    ytest <- append(ytest, data[which(data$index==nexty),"number"])
    
    
    if (data_test$type[i] == "w") {
        x1test <- append(x1test, data_test$inuse_count[i]/n_wash)
    }
    else {
        x1test <- append(x1test, data_test$inuse_count[i]/n_dry)
    }
    
    td <- 0
    dt <- timeofday(data_test$start_time[i])
    if (dt > 6 && dt < 12) {
        td <- 0
    }
    else if (dt > 12 && dt < 18) {
        td <- 1
    }
    else if (dt > 18 && dt < 24) {
        td <- 2
    }
    else {
        td <- 3
    }
    x3test <- append(x3test, td)
    x2test <- append(x2test, as.numeric(data_test$type[i] == "d"))
}

newdata <- data.frame(ytrain=ytest, x1train=x1test, x2train=x2test, x3train=x3test)

harambe <- predict(treemodel, newdata=newdata, type="class")
