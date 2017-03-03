## deprecated -- use coincidence.py instead

data <- as.data.frame(read.csv("../data/hithal.csv"))
names(data) <- c("start_time", "end_time", "extend_time", "idle_time", "number", "type", "dorm")

data$start_time <- as.POSIXct(data$start_time, format="%Y-%m-%d %H:%M:%S")
data$end_time <- as.POSIXct(data$end_time, format="%Y-%m-%d %H:%M:%S")

data <- data[order(data$start_time),]

data <- data[1:1000,]

head(data)

## Get the total time (end_time - start_time + extend_time)

total_time <- as.numeric(difftime(data$end_time, data$start_time), units="mins") + data$extend_time + data$idle_time
data <- cbind(data, total_time)

hist(data$total_time, breaks=seq(0,max(data$total_time)+1, by=0.5), xlim=c(0,120))

i_list <- 800:900#length(data[,1])
j_list <- 1:length(data[,1])

n_inuse = c()
list_inuse = c()
for (i in i_list) {
  n_available = 0
  coin_list = ""

  for (j in j_list) {
    ## Don't check for coincidence for times after
    if (data$start_time[j] >= data$start_time[i]) {
      break # this is ok since ordered
    }
    ## Don't check for coincidence with runs > 2hrs before this one
    if (data$start_time[j] + 120 * 60 < data$start_time[i]) {
        next
    }
    ## Don't check for coincidence from the same machine
    if (data$number[i] == data$number[j]) {
      next
    }
    ## Don't check coincidence for other type of machine
    if (data$type[i] != data$type[j]) {
      next
    }
    
    ## Actually check to see if the machine is available at the time
    if (data$start_time[j] + data$total_time[j] * 60 > data$start_time[i]) {
      n_available = n_available + 1
      if (coin_list != "") {
        coin_list <- paste(coin_list, toString(data$number[j]), sep=",")
      }
      else {
        coin_list <- toString(data$number[j])
      }
    }
  }
  n_inuse <- append(n_inuse, n_available)
  list_inuse <- c(list_inuse, coin_list)
}

