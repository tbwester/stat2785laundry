snehal_data <- read.csv("../data/snehal.csv")
source("distances/snell_dist.R")
source("distances/get_quantile_distances.R")
colnames(snehal_data) <- c("start_time", "end_time", "extend_time", "idle_time", "number", "type", "dorm")

## Show time of day distribution for machine starts
hrs=format(as.POSIXct(snehal_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%H")
min=format(as.POSIXct(snehal_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%M")
sec=format(as.POSIXct(snehal_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%S")
date=format(as.POSIXct(snehal_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%d")
mo=format(as.POSIXct(snehal_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%m")

hrs <-as.numeric(hrs)
min <-as.numeric(min)
sec<-as.numeric(sec)

## Show distribution of machine total times (excluding idle times)
totaltime <- as.numeric(difftime(snehal_data$end_time, snehal_data$start_time), units="mins") + snehal_data$extend_time
totaltime_hr = totaltime/60# in hrs

totaltime_hr

ma_no=snehal_data$number

day=data.frame(cbind(mo, date))

dates=ceiling(as.numeric(difftime(snehal_data$end_time[length(snehal_data[,1])], snehal_data$start_time[1], units="days")))+1

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

means_snell = apply(usage_ma, 1, mean)

dist_snell_pay = c() 
for(i in 1:nrow(usage_ma)){
  d = dist_pay_ma(i, a, b)
  print(d)
  dist_snell_pay = c(dist_snell_pay, d)
}
snell_pay_q = get_quantile_lables(dist_snell_pay)

dist_snell_door = c() 
for(i in 1:nrow(usage_ma)){
  dist_snell_door = c(dist_snell_door, dist_door_ma(i, a, b))
}
snell_door_q = get_quantile_lables(dist_snell_door)

snell_summary = data.frame(usage = means_snell, 
                        pay_q = snell_pay_q,
                        door_q = snell_door_q)
write.csv(snell_summary, "aggregate_data/snell_summary.csv", row.names = FALSE)
