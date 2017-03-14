## functions for machine use prediction
## mostly copied from predictor.R

## merges coincidence files with data files
setupdata <- function(name) {
    ## Get paths to data and coincidence files
    filename <- paste("../data/", name, ".csv", sep = "")
    coinfilename <- paste("../data/coinc/", name, "_coin.txt", sep = "")
    data <- as.data.frame(read.csv(filename, header=FALSE))
    names(data) <- c("start_time", "end_time", "extend_time", "idle_time", "number", "type", "dorm")

    coin_list <- readLines(coinfilename)
    
    ## total number of washers & dryers
    #wash_list <- unique(data$number[which(data$type=='w')])
    #dry_list <- unique(data$number[which(data$type=='d')])
    #n_wash <- length(wash_list)
    #n_dry <- length(dry_list)
    #mac_list <- sort(c(wash_list, dry_list))
    
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
    
    return(data)
}

## Returns a number in the range [0,24] for a given POSIXct date
timeofday <- function(date) {
    hrs=format(as.POSIXct(date, format="%Y-%m-%d %H:%M:%S"), format="%H")
    min=format(as.POSIXct(date, format="%Y-%m-%d %H:%M:%S"), format="%M")
    sec=format(as.POSIXct(date, format="%Y-%m-%d %H:%M:%S"), format="%S")
    
    hrs <-as.numeric(hrs)
    min <-as.numeric(min)
    sec<-as.numeric(sec)
    
    return(hrs + (min / 60) + (sec / 3600))
}

## Returns a vector of machines in-use from a csv string (e.g. ##,##,##)
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

#in_matrix <- function(str, m) {
#    if (str == "") {
#        return(0)
#    }
#    thelist <- strsplit(str, ",")[[1]]
#    count <- 0
#    for (i in 1:length(thelist)) {
#        if (is.element(thelist[i],m)) {
#            count <- count + 1
#        }
#    }
#    return(count)
#}