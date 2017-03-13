fNames = c("hithal", "maxpalwes",
           "burcou", "inthou", "rengrarescom",
           "camnor", "maxpalcen", "snehal", 
           "camnornor", "maxpaleas")

timeofday <- function(date) {
  hrs=format(as.POSIXct(date, format="%Y-%m-%d %H:%M:%S"), format="%H")
  min=format(as.POSIXct(date, format="%Y-%m-%d %H:%M:%S"), format="%M")
  sec=format(as.POSIXct(date, format="%Y-%m-%d %H:%M:%S"), format="%S")
  
  hrs <-as.numeric(hrs)
  min <-as.numeric(min)
  sec<-as.numeric(sec)
  
  return(hrs + (min / 60) + (sec / 3600))
}

setBeggining = function(w_times, offset = 5){
  w_times_o = w_times - offset
  w_times_o[which(w_times_o < 0)] = w_times_o[which(w_times_o < 0)] + 24
  return(w_times_o)
}

get_washer_times = function(data){
  all_times = list()
  wash_list <- unique(data$number[which(data$type=='w')])
  for(w in wash_list){
    #title = paste("washer", w, ",", "hitchcock")
    w_times_day = data$start_time[which(data$number == w)]
    w_times = timeofday(w_times_day)
    all_times[[paste("washer", w, sep = "_")]] = w_times
  }
  return(all_times)
}

get_dryer_times = function(data){
  all_times = list()
  dry_list <- unique(data$number[which(data$type=='d')])
  for(d in dry_list){
    #title = paste("dryer", w, ",", "hitchcock")
    w_times_day = data$start_time[which(data$number == d)]
    w_times = timeofday(w_times_day)
    all_times[[paste("dryer", w, sep = "_")]] = w_times
  }
  return(all_times)
}

ks_machines = function(times){
  n = length(times)
  pvals = matrix(rep(0, n^2), ncol = n)
  for(i in 1:n){
    for(j in i:n){
      ks_res = ks.test(x = times[[i]], y = times[[j]], alternative = "two.sided")
      pvals[i, j] = ks_res$p.value
    }
  }
  colnames(pvals) = names(times)
  rownames(pvals) = names(times)
  return(pvals)
}

i = 1
fileName = paste("../data/", fNames[i], ".csv", sep = "")
data <- as.data.frame(read.csv(fileName))
names(data) <- c("start_time", "end_time", "extend_time", "idle_time", "number", "type", "dorm")
data$start_time = as.POSIXct(data$start_time) 

w_times = get_washer_times(data)
d_times = get_dryer_times(data)

w_ks_p = ks_machines(w_times)
d_ks_p = ks_machines(d_times)
