source("functions.R")
data <- setupdata("burcou")

## total number of washers & dryers
wash_list <- unique(data$number[which(data$type=='w')])
dry_list <- unique(data$number[which(data$type=='d')])
n_wash <- length(wash_list)
n_dry <- length(dry_list)
mac_list <- sort(c(wash_list, dry_list))

data <- data[100:length(data[,1]),] # cut the first 100 entries -- probably too low-desnity
inds <- c() # indicator variables for each machine in-use
for (i in 1:(length(data[,1])-1)) {
    
    data$y[i] <- 0
    for (it in (i+1):length(data[,1])) {
        if (data$type[it] == data$type[i]) {
            dt <- as.numeric(difftime(data$start_time[it],data$start_time[i]), units="mins")
            ## Skip events with no machines of the same type starting over 10 mins later
            if (dt > 10) {
                break
            }
            ## Once a valid machine in found, use it and break
            data$y[i] <- data$number[it]
            break
        }
    }
    
    ## Set up indicator variables
    inds <- rbind(inds, ind_func(data$inuse_list[i]))
    inds[i] = 1 # make sure current machine is included in list of in-use machines
    
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
