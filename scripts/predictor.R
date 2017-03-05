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
x1train <- c() # Predictor 1: Number of machines in use
x2train <- c() # Predictor 2: % in-use of cluster
x3train <- c() # Predictor 3: 0/1 closest to door
x4train <- c() # Predictor 4: time difference (mins) between Y
x5train <- c() # Predictor 5: 0/1 washer/dryer
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
        x2train <- append(x2train, in_matrix(data_train$inuse_list[i], a)/length(a))
    }
    else {
        x1train <- append(x1train, data_train$inuse_count[i]/n_dry)
        x2train <- append(x2train, in_matrix(data_train$inuse_list[i], b)/length(b))
    }
    
    dt <- timeofday(data_train$start_time[i])
    x4train <- append(x4train, dt)
    x5train <- append(x5train, as.numeric(data_train$type[i] == "d"))
}

treemodel <- rpart(ytrain ~ x1train + x2train + x4train + x5train, method="anova",control=rpart.control(minsplit=50, cp=0.001))
plot(treemodel)
text(treemodel)