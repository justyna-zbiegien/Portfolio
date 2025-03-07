#####################################################################
# Funtion returns realizations of y                                 #
# Output:                                                           #                       
# VaRES    - list with a vetor for VaR and ES                       #
# Inputs:                                                           #
# RdrawsC  - matrix (HxNsim) of MC draws                            #                      
# p        - VaR/ES tolerance level                                 #
#####################################################################

RdrawsToVaRES <- function(RDraws,p){
  H    <- dim(Rdraws)[1]
  NSim <- dim(Rdraws)[2]
  RdrawsC <- apply(Rdraws,2,cumsum)               # cumualted changes
  VaR  <- ES <- rep(NaN,H)
  M0  <- floor(Nsim*p)     # observation for p-th quantile 
  for(h in 1:H){
    temp   = sort(RdrawsC[h,])
    VaR[h] = temp[M0]
    ES[h]  = mean(temp[1:M0])
  }
  return(list(VaR=VaR,ES=ES)) 
  }

#####################################################################
# Funtion returns recursive VaR and ES from normal model            #
# Output:                                                           #                       
# VaR   - forecasts for VaR  [zoo]                                  #
# ES    - forecasts for ES  [zoo]                                   #
#                                                                   # 
# Inputs:                                                           #
# y     - data (zoo)                                                #                       
# M     - how many periods for backtesting                          #
# R     - rolling window length                                     #
# p     - VaR tolerance level                                       #
#####################################################################

NORMfct <- function(y, M, R, p=0.05){
  
  T <- length(y)
  # definition of matrices with forecasts
  VaR             <- rep(NA,M)
  ES              <- rep(NA,M)
  
  for (i in 1:M){
    z   <- tail(y[1:(T-1-M+i)],R)
    m   <- mean(z)
    s   <- sd(z)
    VaR[i] <- qnorm(p)*s+m
    ES[i]  <- m - s*dnorm(qnorm(p))/p
  }
  VaR = zoo(VaR,tail(index(y),M))
  ES  = zoo(ES,tail(index(y),M))
  return(list(VaR = VaR, ES = ES)) 
}

#####################################################################
# Output:                                                           #                       
# VaR   - forecasts for VaR  [zoo]                                  #
# ES    - forecasts for ES  [zoo]                                   #
#                                                                   # 
# Inputs:                                                           #
# y     - data (zoo)                                                #                       
# M     - how many periods for backtesting                          #
# R     - rolling window length                                     #
# p     - VaR tolerance level                                       #
#####################################################################

HSfct <- function(y, M, R, p=0.05){
  
  T <- length(y)
  # definition of matrices with forecasts
  VaR             <- rep(NA,M)
  ES              <- rep(NA,M)
  N0              <- floor(R*p)
  
  for (i in 1:M){
    z         <- tail(y[1:(T-1-M+i)],R)
    temp      <- sort(coredata(z))              
    VaR[i]    <- temp[N0]
    ES[i]     <- mean(temp[1:N0])
  }
  VaR = zoo(VaR,tail(index(y),M))
  ES  = zoo(ES,tail(index(y),M))
  return(list(VaR = VaR, ES = ES)) 
}

#####################################################################
# VaR   - forecasts for VaR  [zoo]                                  #
# ES    - forecasts for ES  [zoo]                                   #
#                                                                   # 
# Inputs:                                                           #
# y     - data (zoo)                                                #                       
# M     - how many periods for backtesting                          #
# R     - rolling window length                                     #
# p     - VaR tolerance level                                       #
# v     - degrees of frredom                                        #
#####################################################################


Tfct <- function(y, M, R, p=0.05, v=5){
  require(rugarch)
  T  <- length(y)
  qf <- function(x) qdist("std", p=x, shape=v)
  # definition of matrices with forecasts
  VaR             <- rep(NA,M)
  ES              <- rep(NA,M)
  
  for (i in 1:M){
    z   <- tail(y[1:(T-1-M+i)],R)
    m   <- mean(z)
    s   <- sd(z)
    VaR[i] <- qdist("std",p,shape=v)*s+m
    ES[i]  <- m + s*(1/p * integrate(qf, 0, p)$value)
  }
  VaR = zoo(VaR,tail(index(y),M))
  ES  = zoo(ES,tail(index(y),M))
  return(list(VaR = VaR, ES = ES))
}

#####################################################################
# VaR   - forecasts for VaR  [zoo]                                  #
# ES    - forecasts for ES  [zoo]                                   #
#                                                                   # 
# Inputs:                                                           #
# y     - data (zoo)                                                #                       
# M     - how many periods for backtesting                          #
# R     - rolling window length                                     #
# p     - VaR tolerance level                                       #
# lambda- smoothing parameter                                       #
#####################################################################


EWMAfct <- function(y, M, R, p=0.05, lambda=0.94){
  
  T <- length(y)
  # definition of matrices with forecasts
  VaR             <- rep(NA,M)
  ES              <- rep(NA,M)
  
  for (i in 1:M){
    z      <- tail(y[1:(T-1-M+i)],R)
    m      <- mean(z)
    zstar  <- coredata(z) - m 
    varEWMA     <- rep(0,R)
    varEWMA[1]  <- var(zstar)                        # initialization
    for (t in 2:R){             
      varEWMA[t] = lambda * varEWMA[t-1]  + (1-lambda) * zstar[t-1]^2
    }
    s   <- (lambda * varEWMA[R]  + (1-lambda) * zstar[R]^2)^0.5
    VaR[i] <- qnorm(p)*s+m
    ES[i]  <- m - s*dnorm(qnorm(p))/p
  }
  VaR = zoo(VaR,tail(index(y),M))
  ES  = zoo(ES,tail(index(y),M))
  return(list(VaR = VaR, ES = ES)) 
}


