a=matrix(c(1, 2, 3), ncol = 3, byrow = TRUE)
b=matrix(c(4, 5, 6), ncol=3, byrow = TRUE)

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
  if(num1<=3&&num2>=4){
    nu1=data.frame(which(mw==num1, arr.ind = TRUE))
    nu2=data.frame(which(md==num2, arr.ind = TRUE))
    return(nu2$col+nu1$col+2)
  }else if (num1<=3 && num2<=3){
    return(same_matrix(num1, num2, md))
  }else if (num1 >=4 && num2 <=3){
    nu1=data.frame(which(md==num1, arr.ind = TRUE))
    nu2=data.frame(which(mw==num2, arr.ind = TRUE))
    return(nu2$col+nu1$col+2)
  }else{
    return(same_matrix(num1, num2, mw))
  }
}

##1 and 6 are closest to the door
dist_door_ma <- function(num1, md, mw){
  if(num1<=3){
    return(same_matrix(num1, 1, md)+4)
  }else{
    return(same_matrix(num1, 6, mw)+4)
  }
}

##1 and 6 are closest to the paystation
dist_pay_ma <- function(num1, md, mw){
  if(num1<=3){
    return(same_matrix(num1, 1, md)+5)
  }else{
    return(same_matrix(num1, 6, mw)+3)
  }
}