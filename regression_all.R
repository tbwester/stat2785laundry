burcou_data <- read.csv("aggregate_data/bj_summary.csv")
bj=length(burcou_data[,1])

hithal_data <- read.csv("aggregate_data/hitch_summary.csv")
hitch=length(hithal_data[,1])

maxpcen_data <- read.csv("aggregate_data/maxpc_summary.csv")
maxpc=length(maxpcen_data[,1])

south_data <- read.csv("aggregate_data/south_summary.csv")
south=length(south_data[,1])


snell_data <- read.csv("aggregate_data/snell_summary.csv")
snell=length(snell_data[,1])


stony_data <- read.csv("aggregate_data/stony_summary.csv")
stony=length(stony_data[,1])
aggregate_data=rbind(burcou_data, hithal_data, maxpcen_data, south_data, snell_data, stony_data)
num=c(1:length(aggregate_data[,1]))

aggregate_data=cbind(aggregate_data, num)
vars_pay=c()
for(i in 1:1000){
  fit_bj=lm(usage~pay_q, data=aggregate_data[1:bj,])
  fit_hitch=lm(usage~pay_q, data=aggregate_data[(bj+1):(bj+hitch),])
  fit_maxpc=lm(usage~pay_q, data=aggregate_data[(bj+hitch+1):(bj+hitch+maxpc),])
  fit_south=lm(usage~pay_q, data=aggregate_data[(bj+hitch+maxpc+1):(bj+hitch+maxpc+south),])
  fit_snell=lm(usage~pay_q, data=aggregate_data[(bj+hitch+maxpc+south+1):(bj+hitch+maxpc+south+snell),])
  fit_stony=lm(usage~pay_q, data=aggregate_data[(bj+hitch+maxpc+south++snell+1):(bj+hitch+maxpc+south+snell+stony),])
  pay_coeff=c()
  pay_coeff[1]=fit_bj$coefficients[2]
  pay_coeff[2]=fit_hitch$coefficients[2]
  pay_coeff[3]=fit_maxpc$coefficients[2]
  pay_coeff[4]=fit_south$coefficients[2]
  pay_coeff[5]=fit_snell$coefficients[2]
  pay_coeff[6]=fit_stony$coefficients[2]
  vars_pay[i]=var(pay_coeff)
  if(is.na( var(pay_coeff)) ){
    print("FUCK.")
    print(pay_coeff)
  } 
  ref=aggregate_data$num
  aggregate_data$num=sample(1:length(ref))
  aggregate_data=aggregate_data[order(aggregate_data$num),]
}

boxplot(vars_pay)
hist(vars_pay)
abline(v=vars_pay[1], col="red", lwd=3)
summary(vars_pay)
vars_pay[1]
quantile(vars_pay, vars_pay[1])

aggregate_data=rbind(burcou_data, hithal_data, maxpcen_data, south_data, snell_data, stony_data)
num=c(1:length(aggregate_data[,1]))

vars_door=c()
for(i in 1:1000){
  fit_bj=lm(usage~door_q, data=aggregate_data[1:bj,])
  fit_hitch=lm(usage~door_q, data=aggregate_data[(bj+1):(bj+hitch),])
  fit_maxpc=lm(usage~door_q, data=aggregate_data[(bj+hitch+1):(bj+hitch+maxpc),])
  fit_south=lm(usage~door_q, data=aggregate_data[(bj+hitch+maxpc+1):(bj+hitch+maxpc+south),])
  fit_snell=lm(usage~door_q, data=aggregate_data[(bj+hitch+maxpc+south+1):(bj+hitch+maxpc+south+snell),])
  fit_stony=lm(usage~door_q, data=aggregate_data[(bj+hitch+maxpc+south++snell+1):(bj+hitch+maxpc+south+snell+stony),])
  door_coeff=c()
  door_coeff[1]=fit_bj$coefficients[2]
  door_coeff[2]=fit_hitch$coefficients[2]
  door_coeff[3]=fit_maxpc$coefficients[2]
  door_coeff[4]=fit_south$coefficients[2]
  door_coeff[5]=fit_snell$coefficients[2]
  door_coeff[6]=fit_stony$coefficients[2]
  vars_door[i]=var(door_coeff)
  ref=aggregate_data$num
  aggregate_data$num=sample(1:length(ref))
  aggregate_data=aggregate_data[order(aggregate_data$num),]
}

boxplot(vars_door)
hist(vars_door)
abline(v=vars_door[1], col="red", lwd=3)
summary(vars_door)
vars_door[1]
quantile(vars_door, vars_door[1])



