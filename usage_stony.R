stony_data <- read.csv("data/570stoisl.csv")
source("distances/stony_dist.R")
source("distances/get_quantile_distances.R")
source("quarter and time.R")
colnames(stony_data) <- c("start_time", "end_time", "extend_time", "idle_time", "number", "type", "dorm")

## Show time of day distribution for machine starts
hrs=format(as.POSIXct(stony_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%H")
min=format(as.POSIXct(stony_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%M")
sec=format(as.POSIXct(stony_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%S")
date=format(as.POSIXct(stony_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%d")
mo=format(as.POSIXct(stony_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%m")

hrs <-as.numeric(hrs)
min <-as.numeric(min)
sec<-as.numeric(sec)
date<-as.numeric(date)
mo<-as.numeric(mo)

hist(add_time_seg(hrs))
hist(add_quarter_seg(date, mo))
## Show distribution of machine total times (excluding idle times)
totaltime <- as.numeric(difftime(stony_data$end_time, stony_data$start_time), units="mins") + stony_data$extend_time
totaltime_hr = totaltime/60# in hrs

ma_no=stony_data$number

day=data.frame(cbind(mo, date))

dates=ceiling(as.numeric(difftime(stony_data$end_time[length(stony_data[,1])], stony_data$start_time[1], units="days")))+1

daysum=data.frame(cbind(day, (as.numeric(day$mo)-8)*31-31+as.numeric(day$date)))

colnames(daysum)=c("mo","day","daynum")

for(i in 1:length(totaltime)){
  if(as.numeric(daysum$mo[i])%%2 == 0){
    daysum$daynum[i]=daysum$daynum[i]-1
  }
}

for(i in 1:length(totaltime)){
  if((as.numeric(daysum$mo[i])) >= 11){
    daysum$daynum[i]=daysum$daynum[i]-1
  }
}

day_num=c()
day_num=daysum$daynum-23
usage_ma=matrix(c(rep(0, max(ma_no)*78)), ncol=78, byrow = TRUE)

j=1#ma num 1
k=1#day 1

for(i in 1:length(totaltime)){
  if(ma_no[i]==j){
    if(day_num[i]==k){
      usage_ma[j,k]=usage_ma[j,k]+totaltime_hr[i]
    }else{
      k=k+1
      usage_ma[j,k]=totaltime_hr[i]
    }
  }else{
    j=j+1
    k=1
    usage_ma[j,k]=totaltime_hr[i]
  }
}

colnames(usage_ma)=c(1:78)

means_stony = apply(usage_ma, 1, mean)

dist_stony_pay = c() 
for(i in 1:nrow(usage_ma)){
  dist_stony_pay = c(dist_stony_pay, dist_pay_ma(i, a, b))
}
stony_pay_q = get_quantile_lables(dist_stony_pay)

dist_stony_door = c() 
for(i in 1:nrow(usage_ma)){
  dist_stony_door = c(dist_stony_door, dist_door_ma(i, a, b))
}
stony_door_q = get_quantile_lables(dist_stony_door)

stony_summary = data.frame(usage = means_stony, 
                        pay_q = stony_pay_q,
                        door_q = stony_door_q)
write.csv(stony_summary, "aggregate_data/stony_summary.csv", row.names = FALSE)
