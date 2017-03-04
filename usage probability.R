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

dates=as.numeric(difftime(maxpalcen_data$end_time[length(data[,1])], maxpalcen_data$start_time[1], units="days"))

daysum=as.numeric(day$mo)+as.numeric(day$date)

usage_ma=matrix(c(rep(0, max(ma_no)*78)), ncol=78, byrow = TRUE)

j=1#ma num 1
k=1#day 1
temp=0
#for(i in 1:length(totaltime)){
 ##  if(daysum[i]==daysum[i+1]){
   #   temp=temp+totaltime_hr[i]
    #}else{
     # usage_ma[j][k]=temp+totaltime_hr[i]
      #print(usage_ma[j][k])
      #k=k+1
      #temp=0
    #}
  #}else{
  #  usage_ma[j][k]=temp+totaltime_hr[i]
  # j=j+1
   # k=1
  #  temp=0
#  }
#}
k=1
temp=0
for(i in 1:length(max(ma_no))){
  while(ma_no[k]==ma_no[k+1]){
    for(j in 1:78){
      while(daysum[k]==daysum[k+1]){
        temp=temp+totaltime_hr[k]
        k=k+1
      }
      usage_ma[i][j]=temp+totaltime_hr[k]
      print(temp+totaltime_hr[k])
      k=k+1
      temp=0
    }
    usage_ma[i][j]=temp+totaltime_hr[k]
    print(temp+totaltime_hr[k])
    k=k+1
    temp=0
  }
}


