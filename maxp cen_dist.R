a=matrix(c(3, 4, 5, 6, 7, 8, 9), ncol = 6, byrow = TRUE)
b=matrix(c(10, 12, 14, 16, 18,
           11, 13, 15, 17, 19), ncol=5, byrow = TRUE)
c=matrix(c(1, 2), ncol=2, byrow=TRUE)

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
#3-9 m1, >10 m2 <2 m3
dist_2ma <- function(num1, num2, m1, m2, m3){
  if((num1>=3&&num1<=9)&&(num2>=3&&num2<=9)){
    return(same_matrix(num1, num2, m1))
  }else if((num1>=3&&num1<=9)&&(num2>=10)){
    nu1=data.frame(which(m1==num1, arr.ind = TRUE))
    nu2=data.frame(which(m2==num2, arr.ind = TRUE))
    return (reverse(nu2$row, m2, TRUE)-nu1$row#row
            +reverse(nu1$col, m1, FALSE)-nu2$col+3)#col
  }else if((num1>=3||num1<=9)&&(num2<=2)){
    nu1=data.frame(which(m1==num1, arr.ind = TRUE))
    nu2=data.frame(which(m3==num2, arr.ind = TRUE))
    return (reverse(nu2$col, m3, FALSE)+nu1$col)#col
  }else if ((num1>=10)&&(num2>=3&&num2<=9)){
    nu2=data.frame(which(m1==num2, arr.ind = TRUE))
    nu1=data.frame(which(m2==num1, arr.ind = TRUE))
    return (reverse(nu1$row, m2, TRUE)-nu2$row#row
            +reverse(nu2$col, m1, FALSE)-nu1$col+3)#col
  }else if((num1>=10)&&(num2>=10)){
    return(same_matrix(num1, num2, m2))
  }else if((num1>=10)&&(num2<=2)){
    nu1=data.frame(which(m2==num1, arr.ind = TRUE))
    nu2=data.frame(which(m3==num2, arr.ind = TRUE))
    return(reverse(nu1$row, m2, TRUE)-nu2$row+reverse(nu1$col, m2, FALSE)+nu2$col)
  }else if((num1<=2)&&(num2>=3&&num2<=9)){
    nu2=data.frame(which(m1==num2, arr.ind = TRUE))
    nu1=data.frame(which(m3==num1, arr.ind = TRUE))
    return (reverse(nu1$col, m3, FALSE)+nu2$col)#col
  }else if((num1<=2)&&(num2>=10)){
    nu2=data.frame(which(m2==num2, arr.ind = TRUE))
    nu1=data.frame(which(m3==num1, arr.ind = TRUE))
    return(reverse(nu2$row, m2, TRUE)-nu1$row+reverse(nu2$col, m2, FALSE)+nu1$col)
  }else{
    return(same_matrix(num1, num2, m3))
  }
}




##9, 11, 1  are closest to the door
dist_door_ma <- function(num1, m1, m2, m3){
  if(num1>=3&&num1<=9){
    return(dist_2ma(num1, 9, m1, m2, m3))
  }else if(num1>=10){
    return(dist_2ma(num1, 11, m1, m2, m3)+1)
  }else{
    return(dist_2ma(num1, 1, m1, m2, m3)+6)
  }
}

