a=matrix(c(13, 15, 17, 21, 23, 25, 0, 0, 0, 0,
           14, 16, 18, 20, 22, 24, 26, 1, 2, 3, 4), ncol = 11, byrow = TRUE)
b=matrix(c( 5, 6, 7), ncol=3, byrow = TRUE)
c=matrix(c( 8, 9, 10, 11, 12), ncol=5, byrow = TRUE)

reverse <- function(num, m, isRow){
  if(isRow==TRUE){
    return(-num+nrow(m)+1)    
  }else{
    return(-num+ncol(m)+1) 
  }
}

same_matrix <- function(num1, num2, m){
  nu1=data.frame(which(m==num1, arr.ind = TRUE))
  nu2=data.frame(which(m==num2, arr.ind = TRUE))
  return(abs(nu1$row-nu2$row)+abs(nu1$col-nu2$col))
}

dist_2ma <- function(num1, num2, m1, m2, m3){
  if((num1<=4||num1>=13)&&(num2<=4||num2>=13)){
    return(same_matrix(num1, num2, m1))
  }else if((num1<=4||num1>=13)&&(num2>=5&&num2<=7)){
    nu1=data.frame(which(m1==num1, arr.ind = TRUE))
    nu2=data.frame(which(m2==num2, arr.ind = TRUE))
    return (abs((reverse(nu1$row, m1, TRUE)-nu2$row)#row
            +abs(reverse(nu1$col, m1, FALSE)+nu2$col)))#col
  }else if((num1<=4||num1>=13)&&(num2>=8&&num2<=12)){
    nu1=data.frame(which(m1==num1, arr.ind = TRUE))
    nu2=data.frame(which(m3==num2, arr.ind = TRUE))
    return (reverse(nu1$row, m1, TRUE)-nu2$row#row
            +reverse(nu1$col, m1, FALSE)-nu2$col+3)#col
  }else if ((num1>=5&&num1<=7)&&(num2<=4||num2>=13)){
    nu2=data.frame(which(m1==num2, arr.ind = TRUE))
    nu1=data.frame(which(m2==num1, arr.ind = TRUE))
    return (reverse(nu2$row, m1, TRUE)-nu1$row#row
            +reverse(nu2$col, m1, FALSE)+nu1$col)#col
  }else if((num1>=5&&num1<=7)&&(num2>=5&&num2<=7)){
    return(same_matrix(num1, num2, m2))
  }else if((num1>=5&&num1<=7)&&(num2>=8&&num2<=12)){
    nu1=data.frame(which(m2==num1, arr.ind = TRUE))
    nu2=data.frame(which(m3==num2, arr.ind = TRUE))
    return(reverse(nu1$col, m2, FALSE)+nu2$col)
  }else if((num1>=8&&num1<=12)&&(num2<=4||num2>=13)){
    nu2=data.frame(which(m1==num2, arr.ind = TRUE))
    nu1=data.frame(which(m3==num1, arr.ind = TRUE))
    return (reverse(nu2$row, m1, TRUE)-nu1$row#row
            +reverse(nu2$col, m1, FALSE)-nu1$col+3)#col
  }else if((num1>=8&&num1<=12)&&(num2>=5&&num2<=7)){
    nu2=data.frame(which(m2==num2, arr.ind = TRUE))
    nu1=data.frame(which(m3==num1, arr.ind = TRUE))
    return(reverse(nu2$col, m2, FALSE)+nu1$col)
  }else{
    return(same_matrix(num1, num2, m3))
    }
}
    
    
    

##14, 7, 11  are closest to the door
dist_door_ma <- function(num1, m1, m2, m3){
  if(num1<=4||num1>=13){
    return(dist_2ma(num1, 12, m1, m2, m3)+4)
  }else if(num1>=5&&num1<=7){
    return(dist_2ma(num1, 7, m1, m2, m3)+10)
  }else{
    return(dist_2ma(num1, 11, m1, m2, m3)+5)
  }
}

