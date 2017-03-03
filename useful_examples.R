############################
### Some Nice R Examples ###
############################

## load the data
data <- as.data.frame(read.csv("../data/rengrarescom.csv"))
names(data) <- c("start_time", "end_time", "extend_time", "idle_time", "number", "type", "dorm")

## sort data by start time
data <- data[order(data$start_time),]

## get total number of days in quarter
as.numeric(difftime(data$end_time[length(data[,1])], data$start_time[1], units="days"))
