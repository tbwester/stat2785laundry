a=matrix(c(1,3,5,
           2,4,6), ncol = 3, byrow = TRUE)
b=matrix(c(7,8,9), ncol=3, byrow = TRUE)
c=matrix(c(10, 11), ncol=2, byrow=TRUE)

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
  if((num1<=6)&&(num2<=6)){
    return(same_matrix(num1, num2, m1))
  }else if((num1<=6)&&(num2>=7&&num2<=9)){
    nu1=data.frame(which(m1==num1, arr.ind = TRUE))
    nu2=data.frame(which(m2==num2, arr.ind = TRUE))
    return (reverse(nu1$row, m1, TRUE)-nu2$row#row
            +nu1$col-nu2$col)#col
  }else if((num1<=6)&&(num2>=10)){
    nu1=data.frame(which(m1==num1, arr.ind = TRUE))
    nu2=data.frame(which(m3==num2, arr.ind = TRUE))
    return (reverse(nu1$row, m1, TRUE)-nu2$row#row
            +reverse(nu1$col, m1, FALSE)-nu2$col+4)#col
  }else if ((num1>=7&&num1<=9)&&(num2<=6)){
    nu2=data.frame(which(m1==num2, arr.ind = TRUE))
    nu1=data.frame(which(m2==num1, arr.ind = TRUE))
    return (reverse(nu2$row, m2, TRUE)-nu1$row#row
            +nu2$col-nu1$col)#col
  }else if((num1>=7&&num1<=9)&&(num2>=7&&num2<=9)){
    return(same_matrix(num1, num2, m2))
  }else if((num1>=7&&num1<=9)&&(num2>=10)){
    nu1=data.frame(which(m2==num1, arr.ind = TRUE))
    nu2=data.frame(which(m3==num2, arr.ind = TRUE))
    return(reverse(nu1$col, m2, FALSE)+nu2$col)
  }else if((num1>=10)&&(num2<=6)){
    nu2=data.frame(which(m1==num2, arr.ind = TRUE))
    nu1=data.frame(which(m3==num1, arr.ind = TRUE))
    return (reverse(nu2$row, m2, TRUE)-nu1$row#row
            +reverse(nu2$col, m2, FALSE)-nu1$col+4)#col
  }else if((num1>=10)&&(num2>=7&&num2<=9)){
    nu2=data.frame(which(m2==num2, arr.ind = TRUE))
    nu1=data.frame(which(m3==num1, arr.ind = TRUE))
    return(reverse(nu2$co2, m2, FALSE)+nu1$col)
  }else{
    return(same_matrix(num1, num2, m3))
  }
}


##1, 9, 11  are closest to the door
dist_door_ma <- function(num1, m1, m2, m3){
  if(num1<=6){
    return(same_matrix(num1, 1, m1))
  }else if(num1>=7&&num1<=9){
    return(same_matrix(num1, 9, m2)+1)
  }else{
    return(same_matrix(num1, 11, m3)+6)
  }
}

