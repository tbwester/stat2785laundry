rengrarescom_data <- read.csv("data/rengrarescom.csv")
source("distances/south_dist.R")
source("distances/get_quantile_distances.R")
source("quarter and time.R")
colnames(rengrarescom_data) <- c("start_time", "end_time", "extend_time", "idle_time", "number", "type", "dorm")

## Show time of day distribution for machine starts
hrs=format(as.POSIXct(rengrarescom_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%H")
min=format(as.POSIXct(rengrarescom_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%M")
sec=format(as.POSIXct(rengrarescom_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%S")
date=format(as.POSIXct(rengrarescom_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%d")
mo=format(as.POSIXct(rengrarescom_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%m")

hrs <-as.numeric(hrs)
min <-as.numeric(min)
sec<-as.numeric(sec)
date<-as.numeric(date)
mo<-as.numeric(mo)

hist(add_time_seg(hrs))
hist(add_quarter_seg(date, mo))
## Show distribution of machine total times (excluding idle times)
totaltime <- as.numeric(difftime(rengrarescom_data$end_time, rengrarescom_data$start_time), units="mins") + rengrarescom_data$extend_time
totaltime_hr = totaltime/60# in hrs

totaltime_hr

ma_no=rengrarescom_data$number

day=data.frame(cbind(mo, date))

dates=ceiling(as.numeric(difftime(rengrarescom_data$end_time[length(rengrarescom_data[,1])], rengrarescom_data$start_time[1], units="days")))+1

daysum=data.frame(cbind(day, as.numeric(day$mo)*31-31+as.numeric(day$date)))

colnames(daysum)=c("mo","day","daynum")

for(i in 1:length(totaltime)){
  if(as.numeric(daysum$mo[i])%%2 == 0){
    daysum$daynum[i]=daysum$daynum[i]-1
  }
}

for(i in 1:length(totaltime)){
  if((as.numeric(daysum$mo[i])+8) >= 11){
    daysum$daynum[i]=daysum$daynum[i]-1
  }
}

day_num=c()
day_num=daysum$daynum-23
usage_ma=matrix(c(rep(0, max(ma_no)*78)), ncol=78, byrow = TRUE)

j=1#ma num 1
k=1#day 1
temp=0

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

means_south = apply(usage_ma, 1, mean)

dist_south_pay = c() 
for(i in 1:nrow(usage_ma)){
  dist_south_pay = c(dist_south_pay, dist_pay_ma(i, a, b, c, d, e, f, g, h))
}
south_pay_q = get_quantile_lables(dist_south_pay)

dist_south_door = c() 
for(i in 1:nrow(usage_ma)){
  dist_south_door = c(dist_south_door, dist_door_ma(i, a, b, c, d,
                                                    e, f, g, h))
}
south_door_q = get_quantile_lables(dist_south_door)

length(means_south)
length(south_pay_q)
south_summary = data.frame(usage = means_south, 
                        pay_q = south_pay_q,
                        door_q = south_door_q)
write.csv(south_summary, "aggregate_data/south_summary.csv", row.names = FALSE)