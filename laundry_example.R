maxpalcen_data <- read.csv("../data/maxpalcen.csv")
colnames(maxpalcen_data) <- c("start_time", "end_time", "extend_time", "idle_time", "number", "type", "dorm")

## Show the distribution of idle times from 0 mins to 2 hrs, in bins of width = 30 seconds
hist(maxpalcen_data$idle_time, breaks=seq(0,max(maxpalcen_data$idle_time), by=0.5), xlim=c(0,120))

## Show distribution of machine use
hist(maxpalcen_data$number)

## Show time of day distribution for machine starts
hrs=format(as.POSIXct(maxpalcen_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%H")
min=format(as.POSIXct(maxpalcen_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%M")
sec=format(as.POSIXct(maxpalcen_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%S")

hrs <-as.numeric(hrs)
min <-as.numeric(min)
sec<-as.numeric(sec)

hist(hrs + (min / 60) + (sec / 3600))

## Show distribution of machine total times (excluding idle times)
totaltime <- as.numeric(difftime(maxpalcen_data$end_time, maxpalcen_data$start_time), units="mins") + maxpalcen_data$extend_time
hist(totaltime, breaks=seq(0,max(totaltime), by=0.5), xlim=c(0,120))
