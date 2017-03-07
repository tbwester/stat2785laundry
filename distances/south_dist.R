a=matrix(c(1,2,3,4, 5, 6, 7, 8, 9), ncol=9, byrow = TRUE)
b=matrix(c(18, 16, 14, 12, 10,
           19, 17, 15, 13, 11), ncol=5, byrow = TRUE)
c=matrix(c(24, 22,20,
           25, 23, 21), ncol=3, byrow = TRUE)
d=matrix(c(30, 29, 28, 27, 26), ncol=5, byrow = TRUE)
e=matrix(c(31, 32, 33, 34), ncol=4, byrow = TRUE)
f=matrix(c(35, 37,
           36, 38), ncol=2, byrow = TRUE)
g=matrix(c(39,41,43,45,47,
           40,42,44,46,48), ncol=5, byrow = TRUE)
h=matrix(c(58, 57, 56, 55, 54, 53,52, 51, 50, 49), ncol=10, byrow = TRUE)

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
#############
#dist_2ma <- function(num1, num2, m1, m2, m3){
 # if((num1<=6)&&(num2<=6)){
#    return(same_matrix(num1, num2, m1))
#  }else if((num1<=6)&&(num2>=7&&num2<=9)){
   # nu1=data.frame(which(m1==num1, arr.ind = TRUE))
  #  nu2=data.frame(which(m2==num2, arr.ind = TRUE))
  #  return (reverse(nu1$row, m1, TRUE)-nu2$row#row
   #         +nu1$col-nu2$col)#col
  #}else if((num1<=6)&&(num2>=10)){
  #  nu1=data.frame(which(m1==num1, arr.ind = TRUE))
  #  nu2=data.frame(which(m3==num2, arr.ind = TRUE))
  #  return (reverse(nu1$row, m1, TRUE)-nu2$row#row
  #          +reverse(nu1$col, m1, FALSE)-nu2$col+4)#col
  #}else if ((num1>=7&&num1<=9)&&(num2<=6)){
  #  nu2=data.frame(which(m1==num2, arr.ind = TRUE))
  #  nu1=data.frame(which(m2==num1, arr.ind = TRUE))
  #  return (reverse(nu2$row, m2, TRUE)-nu1$row#row
  #          +nu2$col-nu1$col)#col
  #}else if((num1>=7&&num1<=9)&&(num2>=7&&num2<=9)){
   # return(same_matrix(num1, num2, m2))
  #}else if((num1>=7&&num1<=9)&&(num2>=10)){
 #   nu1=data.frame(which(m2==num1, arr.ind = TRUE))
#    nu2=data.frame(which(m3==num2, arr.ind = TRUE))
 #   return(reverse(nu1$col, m2, FALSE)+nu2$col)
#  }else if((num1>=10)&&(num2<=6)){
  #  nu2=data.frame(which(m1==num2, arr.ind = TRUE))
  #  nu1=data.frame(which(m3==num1, arr.ind = TRUE))
  #  return (reverse(nu2$row, m2, TRUE)-nu1$row#row
  #          +reverse(nu2$col, m2, FALSE)-nu1$col+4)#col
  #}else if((num1>=10)&&(num2>=7&&num2<=9)){
  #  nu2=data.frame(which(m2==num2, arr.ind = TRUE))
  #  nu1=data.frame(which(m3==num1, arr.ind = TRUE))
  #  return(reverse(nu2$co2, m2, FALSE)+nu1$col)
  #}else{
  #  return(same_matrix(num1, num2, m3))
 # }
#}
######

##1, 19, 25, 30, 31,38,48,58  are closest to the door
dist_door_ma <- function(num1, m1, m2, m3, m4, m5, m6, m7, m8){
  if(num1<=9){
    return(same_matrix(num1, 1, m1)+18)
  }else if(num1>=10&&num1<=19){
    return(same_matrix(num1, 19, m2)+17)
  }else if(num1>=20&&num1<=25){
    return(same_matrix(num1,25, m3)+14)
  }else if(num1>=26&&num1<=30){
    return(same_matrix(num1, 30, m4)+10)
  }else if(num1>=31&&num1<=34){
    return(same_matrix(num1, 31, m5)+8)
  }else if(num1>=35&&num1<=38){
    return(same_matrix(num1, 38, m6)+6)
  }else if(num1>=39&&num1<=48){
    return(same_matrix(num1, 48, m7)+7)
  }else if(num1>=49&&num1<=58){
    return(same_matrix(num1, 58, m8)+1)
  }
}
##1, 19, 25, 30, 31,38,48,58  are closest to the pay
dist_pay_ma <- function(num1, m1, m2, m3, m4, m5, m6, m7, m8){
  if(num1<=9){
    return(same_matrix(num1, 1, m1)+15)
  }else if(num1>=10&&num1<=19){
    return(same_matrix(num1, 19, m2)+12)
  }else if(num1>=20&&num1<=25){
    return(same_matrix(num1,25, m3)+9)
  }else if(num1>=26&&num1<=30){
    return(same_matrix(num1, 30, m4)+7)
  }else if(num1>=31&&num1<=34){
    return(same_matrix(num1, 31, m5)+8)
  }else if(num1>=35&&num1<=38){
    return(same_matrix(num1, 38, m6)+6)
  }else if(num1>=39&&num1<=48){
    return(same_matrix(num1, 48, m7)+7)
  }else if(num1>=49&&num1<=58){
    return(same_matrix(num1, 58, m8)+2)
  }
}