maxpalwes_data <- read.csv("data/maxpalwes.csv")

source("distances/hitch_dist.R")
source("distances/get_quantile_distances.R")
source("quarter and time.R")
colnames(maxpalwes_data) <- c("start_time", "end_time", "extend_time", "idle_time", "number", "type", "dorm")

## Show time of day distribution for machine starts
hrs=format(as.POSIXct(maxpalwes_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%H")
min=format(as.POSIXct(maxpalwes_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%M")
sec=format(as.POSIXct(maxpalwes_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%S")
date=format(as.POSIXct(maxpalwes_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%d")
mo=format(as.POSIXct(maxpalwes_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%m")

hrs <-as.numeric(hrs)
min <-as.numeric(min)
sec<-as.numeric(sec)
date<-as.numeric(date)
mo<-as.numeric(mo)

hist(add_time_seg(hrs))
hist(add_quarter_seg(date, mo))

## Show distribution of machine total times (excluding idle times)
totaltime <- as.numeric(difftime(maxpalwes_data$end_time, maxpalwes_data$start_time), units="mins") + maxpalwes_data$extend_time
totaltime_hr = totaltime/60# in hrs

totaltime_hr

ma_no=maxpalwes_data$number

day=data.frame(cbind(mo, date))

dates=ceiling(as.numeric(difftime(maxpalwes_data$end_time[length(maxpalwes_data[,1])], maxpalwes_data$start_time[1], units="days")))+1

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

