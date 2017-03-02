maxpalcen_data <- read.csv("../data/maxpalcen.csv")
colnames(maxpalcen_data) <- c("start_time", "end_time", "extend_time", "idle_time", "number", "type", "dorm")

## Show time of day distribution for machine starts
hrs=format(as.POSIXct(maxpalcen_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%H")
min=format(as.POSIXct(maxpalcen_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%M")
sec=format(as.POSIXct(maxpalcen_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%S")
date=format(as.POSIXct(maxpalcen_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%d")
mo=format(as.POSIXct(maxpalcen_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%m")

hrs <-as.numeric(hrs)
min <-as.numeric(min)
sec<-as.numeric(sec)

## Show distribution of machine total times (excluding idle times)
totaltime <- as.numeric(difftime(maxpalcen_data$end_time, maxpalcen_data$start_time), units="mins") + maxpalcen_data$extend_time
totaltime_hr = totaltime/60# in hrs

ma_no=maxpalcen_data$number
ma_no[length(ma_no)+1]=0
mo[length(mo)+1]=0
date[length(date)+1]=0

day=data.frame(cbind(mo, date))

daysum=as.numeric(day$mo)+as.numeric(day$date)

usage_ma=matrix(c(rep(0, max(ma_no)*78)), ncol=78, byrow = TRUE)

j=1#ma num 1
k=1#day 1

for(i in 1:length(totaltime)){
  if (ma_no[i]==ma_no[i+1]){
    if(daysum[i]==daysum[i+1]){
      usage_ma[j][k]=usage_ma[j][k]+totaltime_hr[i]
    }else{
      usage_ma[j][k]=usage_ma[j][k]+totaltime_hr[i]
      k=k+1
    }
  }else{
    usage_ma[j][k]=usage_ma[j][k]+totaltime_hr[i]
    j=j+1
    k=1
  }
}

