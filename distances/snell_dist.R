totaltime <- as.numeric(difftime(snell_data$end_time, snell_data$start_time), units="mins") + snell_data$extend_time

a=matrix(c(1, 3,
          2, 4), ncol = 2, byrow = TRUE)
b=matrix(c(5, 6, 7), ncol=3, byrow = TRUE)

reverse <- function(num, m, isRow){
  if(isRow==TRUE){
    return(-num+nrow(m))    
  }else{
   return(-num+ncol(m)) 
  }
}

same_matrix <- function(num1, num2, m){
  nu1=data.frame(which(m==num1, arr.ind = TRUE))
  nu2=data.frame(which(m==num2, arr.ind = TRUE))
  return(abs(nu1$row-nu2$row)+abs(nu1$col-nu2$col))
}

dist_2ma <- function(num1, num2, md, mw){
  if(num1<=4&&num2>=5){
    nu1=data.frame(which(md==num1, arr.ind = TRUE))
    nu2=data.frame(which(mw==num2, arr.ind = TRUE))
    return((abs(nu1$row-(nu2$row+1))+1)+#row layout
      nu2$col+reverse(nu1$col, md, "FALSE")+1)#col laytout
  }else if (num1<=4 && num2<=4){
    return(same_matrix(num1, num2, md))
  }else if (num1 >=5 && num2 <=4){
    nu1=data.frame(which(mw==num1, arr.ind = TRUE))
    nu2=data.frame(which(md==num2, arr.ind = TRUE))
    return((abs(nu2$row-(nu1$row+1))+1)+#row layout
             nu1$col+reverse(nu2$col, md, "FALSE")+1)#col laytout
  }else{
    return(same_matrix(num1, num2, mw))
  }
}

##2 and 7 are closest to the door
dist_door_ma <- function(num1, md, mw){
  if(num1<=4){
    return(same_matrix(num1, 2, md)+3)
  }else{
    return(same_matrix(num1, 7, mw)+3)
  }
}

##2 and 7 are closest to the paystation
dist_pay_ma <- function(num1, md, mw){
  if(num1<=4){
    return(same_matrix(num1, 2, md)+2)
  }else{
    return(same_matrix(num1, 7, mw)+2)
  }
}