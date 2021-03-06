maxpalcen_data <- read.csv("data/maxpalcen.csv")
source("distances/maxp_cen_dist.R")
source("distances/get_quantile_distances.R")
colnames(maxpalcen_data) <- c("start_time", "end_time", "extend_time", "idle_time", "number", "type", "dorm")

## Show time of day distribution for machine starts
hrs=format(as.POSIXct(maxpalcen_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%H")
min=format(as.POSIXct(maxpalcen_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%M")
sec=format(as.POSIXct(maxpalcen_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%S")
date=format(as.POSIXct(maxpalcen_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%d")
mo=format(as.POSIXct(maxpalcen_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%m")
year=format(as.POSIXct(maxpalcen_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%Y")

hrs <-as.numeric(hrs)
min <-as.numeric(min)
sec<-as.numeric(sec)
date<-as.numeric(date)
mo<-as.numeric(mo)
source("quarter and time.R")

maxp_cen_quarter=add_quarter_seg(date, mo)

hist(maxp_cen_quarter)


maxp_cen_time=add_time_seg(hrs)

hist(maxp_cen_time)

## Show distribution of machine total times (excluding idle times)
totaltime <- as.numeric(difftime(maxpalcen_data$end_time, maxpalcen_data$start_time), units="mins") + maxpalcen_data$extend_time
totaltime_hr = totaltime/60# in hrs

ma_no=maxpalcen_data$number

day=data.frame(cbind(mo, date))

dates=ceiling(as.numeric(difftime(maxpalcen_data$end_time[length(maxpalcen_data[,1])], maxpalcen_data$start_time[1], units="days")))+1

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
usage_ma=matrix(c(rep(0, max(ma_no)*dates)), ncol=dates, byrow = TRUE)

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

colnames(usage_ma)=c(1:dates)

means_maxpc = apply(usage_ma, 1, mean)

dist_maxpc_pay = c() 
for(i in 1:nrow(usage_ma)){
  dist_maxpc_pay = c(dist_maxpc_pay, dist_pay_ma(i, a, ab, c))
}
maxpc_pay_q = get_quantile_lables(dist_maxpc_pay)

dist_maxpc_door = c() 
for(i in 1:nrow(usage_ma)){
  dist_maxpc_door = c(dist_maxpc_door, dist_door_ma(i, a, ab, c))
}
maxpc_door_q = get_quantile_lables(dist_maxpc_door)

maxpc_summary = data.frame(usage = means_maxpc, 
                        pay_q = maxpc_pay_q,
                        door_q = maxpc_door_q)
write.csv(maxpc_summary, "aggregate_data/maxpc_summary.csv", row.names = FALSE)

