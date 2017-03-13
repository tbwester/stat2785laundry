
##Use after you read the data
date=format(as.POSIXct(maxpalcen_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%d")
mo=format(as.POSIXct(maxpalcen_data$start_time, format="%Y-%m-%d %H:%M:%S"), format="%m")
date<-as.numeric(date)
mo<-as.numeric(mo)

## Divide into differnet part of time

##0-6 Dust
##6-12 Morning
##12-18 Afternoon
##18-24 Night

diff_time_day <-function(hrs){
  if(hrs>=0&&hrs<6){
    return(1)
  }else if(hrs>=6&&hrs<12){
    return(2)
  }else if (hrs>=12&&hrs<18){
    return(3)
  }else if(hrs>=18&&hrs<24){
    return(4)
  }
}

add_time_seg<-function(hrs){
  time=c()  
  for(i in 1:length(hrs)){
    time[i]=diff_time_day(hrs[i])
  }
  return(time)
}

##Start to devide the machines into different time of the quarter
##Quarter beginer Sep.24-Oct.14 Wk1 to wk3
##Midterms Oct.15-Nov.4 Wk 4 to wk6
##Thanksgiving Nov.5-Nov.25 wk7 to wk 9
##Reading period and Finals Nov.26 - Dec.10

diff_part_quarter <-function(date, mo){
  if (mo==9){
    return(1)
  }else if (mo==10&&date<=7){
    return(2)
  }else if (mo==10 && date<=14){
    return(3)
  }else if (mo==10 && date<=21){
    return(4)
  }else if(mo==10 && date<=28){
    return(5)
  }else if(mo==10 || (mo==11&&date<=4)){
    return(6)
  }else if(mo==11 && date<=11){
    return(7)
  }else if(mo==11 && date<=18){
    return(8)
  }else if(mo==11 && date<=25){
    return(9)
  }else if(mo==11 || (mo==12 && date<=2)){
    return(10)
  }else{
    return(11)
  }
}
day=data.frame(cbind(mo, date))

dates=ceiling(as.numeric(difftime(burcou_data$end_time[length(burcou_data[,1])], burcou_data$start_time[1], units="days")))+1

daysum=data.frame(cbind(day, as.numeric(day$mo)*31-31+as.numeric(day$date)))

colnames(daysum)=c("mo","day","daynum
                   ")

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
quarter.grp = cut(day_num,
                  breaks= c(7,14,21, 28, 35, 42, 49, 56, 63, 70))

add_quarter_seg<-function(date,mo){
  quarter=c()  
  for(i in 1:length(date)){
    quarter[i]=diff_part_quarter(date[i],mo[i])
  }
  return(quarter)
}

