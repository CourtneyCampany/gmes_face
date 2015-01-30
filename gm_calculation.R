
RespBernacchi_func <- function(x, c=18.72, H=46.39, R25c = 1.07, R=8.314472) {
  
  x$Respiration <- R25c * exp(c-H*1000/(R*(273+x$Tleaf)))
  x$Rfit <- R25C * (exp(c-h*1000/(R*(273+x$Tleaf))))
}
  





gmcalc_func <- function(x, a=4.4, ab= 2.9, e=30, b=29, f=16.2,del_growth = -8 , delR=-38){
    x$CiCa <- x$Ci/x$CO2R
    x$a_prime <- (ab*(x$CO2S-x$C2sfc)+a*(x$C2sfc-x$Ci))/(x$CO2S-x$Ci)
    #x$R <- 
    #x$Gstar <-  exp() 
    x$rd_term <- e*x$R*(x$Ci-x$Gstar)/((x$Photo+x$R))*x$CO2S
    x$f_\term <- f*x$Gstar/x$CO2S
    x$Tleaf-Tair <- x$Tleaf - x$Tair
    x$Tblock-Tair <- x$Tblk - x$Tair
    x$CO2Rdry <- x$CO2R/(1-x$H2OR/1000)
    x$CO2Sdry <- x$CO2S/(1-x$H2OS/1000)
    x$t <- (1+x$a_prime/1000)*x$Trmmol/x$CndCO2/1000/2
    x$(one/one-t) <- 1/(1-x$t)
    x$(one+t/one-t) <- (1+x$t)/(1-x$t)
    x$Di <- x$a_prime * x$(one/one-t)+(x$(one+t/one-t) * b-x$a_prime * x$(one/one-t))*x$CiCa
    x$Di-Do <- x$Di - x$Delta
    x$R_term <- x$(one+t/one-t) - x$Rd_term
    x$f_term2 <- x$(one+t/one-t) - x$f_term
    
    x$gm <- x$(one+t/one-t) * (b - 1.8 - x$R * e / (x$R+x$Photo)) * x$Photo/x$CO2S/(x$Di-Do - x$R_term - x$f_term2)
    x$gm_bar <- x$gm*100/x$Press
}