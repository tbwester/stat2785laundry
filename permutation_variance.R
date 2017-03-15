source("usage_burcou.R", echo = T)
source("usage_hithal.R", echo = T)
source("usage_maxpcen.R", echo = T)
source("usage_rengrarescom.R", echo = T)
source("usage_snehal.R", echo = T)
source("usage_stony.R", echo = T)
pay_coeff=c()
pay_coeff[1]=fit_bj$coefficients[2]
pay_coeff[2]=fit_hitch$coefficients[2]
pay_coeff[3]=fit_maxpc$coefficients[2]
pay_coeff[4]=fit_south$coefficients[2]
pay_coeff[5]=fit_snell$coefficients[2]
pay_coeff[6]=fit_stony$coefficients[2]

var_permu=var(pay_coeff)

