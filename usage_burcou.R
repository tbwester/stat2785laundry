#SET THE WORKING DIRECTORY TO HERE 
burcou_data <- read.csv("data/burcou.csv")
source("distances/bj_dist.R")
source("distances/get_quantile_distances.R")
source("quarter and time.R")
colnames(burcou_data) <- c("start_time", "end_time", "extend_time", "idle_time", "number", "type", "dorm")

## Show time of day distribution for machine starts
hrs=format(as.POSIXct(burcou_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%H")
min=format(as.POSIXct(burcou_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%M")
sec=format(as.POSIXct(burcou_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%S")
date=format(as.POSIXct(burcou_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%d")
mo=format(as.POSIXct(burcou_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%m")

hrs <-as.numeric(hrs)
min <-as.numeric(min)
sec<-as.numeric(sec)
date<-as.numeric(date)
mo<-as.numeric(mo)

hist(add_time_seg(hrs))

histPercent <- function(date, mo) {
  H <- hist(add_quarter_seg(date, mo), plot = FALSE)
  H$density <- with(H, 100 * density* diff(breaks)[1])
  labs <- paste(round(H$density), "%", sep="")
  plot(H, freq = FALSE, xlim=c(1, 11),labels=labs, main="Distribution of usage density among different time of the quarter", xlab = "Week Number", col ="lightskyblue")
}

histPercent(date, mo)

p=hist(add_quarter_seg(date, mo), xlim=c(1, 11), xlab = "Week Number", col ="lightskyblue", labels = T) 

## Show distribution of machine total times (excluding idle times)
totaltime <- as.numeric(difftime(burcou_data$end_time, burcou_data$start_time), units="mins") + burcou_data$extend_time
totaltime_hr = totaltime/60# in hrs

totaltime_hr

ma_no=burcou_data$number

day=data.frame(cbind(mo, date))

dates=ceiling(as.numeric(difftime(burcou_data$end_time[length(burcou_data[,1])], burcou_data$start_time[1], units="days")))+1

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

means_bj = apply(usage_ma, 1, mean)

dist_bj_pay = c() 
for(i in 1:nrow(usage_ma)){
  dist_bj_pay = c(dist_bj_pay, dist_pay_ma(i, a, b))
}
bj_pay_q = get_quantile_lables(dist_bj_pay)

dist_bj_door = c() 
for(i in 1:nrow(usage_ma)){
  dist_bj_door = c(dist_bj_door, dist_door_ma(i, a, b))
}
bj_door_q = get_quantile_lables(dist_bj_door)

bj_summary = data.frame(usage = means_bj, 
                        pay_q = bj_pay_q,
                        door_q = bj_door_q)
write.csv(bj_summary, "aggregate_data/bj_summary.csv", row.names = FALSE)

