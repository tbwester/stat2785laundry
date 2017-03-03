## merges coincidence files with data files

data <- as.data.frame(read.csv("../data/rengrarescom.csv"))
names(data) <- c("start_time", "end_time", "extend_time", "idle_time", "number", "type", "dorm")

coin_list <- readLines("../data/coinc/rengrarescom_coin.txt")

## total number of washers & dryers
n_wash <- length(unique(data$number[which(data$type=='w')]))
n_dry <- length(unique(data$number[which(data$type=='d')]))

coin_count <- c()
for (i in 1:length(coin_list)) {
    coin_count <- append(coin_count, length(strsplit(coin_list[i], ",")[[1]]))
}

## sort data
data <- data[order(data$start_time),]

## merge
data$inuse_list <- head(coin_list, -1)
data$inuse_count <- head(coin_count, -1)

hist(data$inuse_count)