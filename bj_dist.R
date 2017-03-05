a=matrix(c(1, 3, 5, 7, 9, 0, 0, 0, 0, 0, 0,
           2, 4, 6, 8, 10, 11, 12, 13, 14, 15, 16), ncol = 11, byrow = TRUE)
b=matrix(c(17, 18), ncol=2, byrow = TRUE)

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

dist_2ma <- function(num1, num2, m1, m2){
  if(num1<=16&&num2>=17){
    nu1=data.frame(which(m1==num1, arr.ind = TRUE))
    nu2=data.frame(which(m2==num2, arr.ind = TRUE))
    return (reverse(nu1$rol, m1, TRUE)+nu2$rol#rol
            +same_matrix(num1, 13, m1)+5+reverse(num2, m2, FALSE))#col
    (nu2$col+nu1$col+2)
    
  }else if (num1<=16 && num2<=16){
    return(same_matrix(num1, num2, m1))
  }else if (num1 >=17 && num2 <=16){
    nu1=data.frame(which(m2==num1, arr.ind = TRUE))
    nu2=data.frame(which(m1==num2, arr.ind = TRUE))
    return (reverse(nu2$rol, m1, TRUE)+nu1$rol#rol
            +same_matrix(num2, 13, m1)+5+reverse(num1, m2, FALSE))#col
  }else{
    return(same_matrix(num1, num2, m2))
  }
}

##11, 18  are closest to the door
dist_door_ma <- function(num1, m1, m2){
  if(num1<=16){
    return(same_matrix(num1, 11, m1)+5)
  }else{
    return(same_matrix(num1, 18, m2)+1)
  }
}

##9, 18  are closest to the paystation
dist_pay_ma <- function(num1, m1, m2){
  if(num1<=16){
    return(same_matrix(num1, 9, m1)+5)
  }else{
    return(same_matrix(num1, 18, m2)+2)
  }
}

