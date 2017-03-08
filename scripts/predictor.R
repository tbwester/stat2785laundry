## script to predict next machine to be used

## merges coincidence files with data files
data <- as.data.frame(read.csv("../data/snehal.csv"))
names(data) <- c("start_time", "end_time", "extend_time", "idle_time", "number", "type", "dorm")

coin_list <- readLines("../data/coinc/snehal_coin.txt")

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
data <- data[100:length(data[,1]),] # cut the first and last 100 entries -- probably too low-desnity

for (i in 1:(length(data[,1])-1)) {
    
    data$y[i] <- 0
    invalid = FALSE
    for (it in (i+1):length(data[,1])) {
        if (data$type[it] == data$type[i]) {
          data$y[i] <- data$number[it]
          break
        }
    }
        
    if (data$type[i] == "w") {
        data$x1[i] <- data$inuse_count[i]/n_wash
    }
    else {
        data$x1[i] <- data$inuse_count[i]/n_dry
    }
    
    td <- 0
    dt <- timeofday(data$start_time[i])
    td <- floor(dt)
    data$x3[i] <- td
    data$x2[i] <- as.numeric(data$type[i] == "d")
    
}

samp <- sample(length(data[,1]),1000) ## get 1000 random points
data_train <- data[samp,]
data_test <- data[-samp,]

#treemodel <- rpart(data_train$y ~ data_train$x1 + data_train$x2 + data_train$x3, method="class",control=rpart.control(minsplit=50, cp=0.001))
treemodel <- rpart(y ~ x1 + x2 + x3, data=data_train, method="class",control=rpart.control(minsplit=50, cp=0.001))
#rfc_tr = randomForest(as.factor(y) ~ x1 + x2 + x3, data=data_train)
plot(treemodel)
text(treemodel)

#res_rfc = predict(rfc_tr, newdata = data_test, type="class")
#cat("RANDOM FOREST:", mean(res_rfc == data_test$y) )

results <- predict(treemodel, type="class")
sum(results==data_train$y) / length(results)

results_test <- predict(treemodel, newdata=data_test, type="class")
sum(results_test==data_test$y) / length(results_test)