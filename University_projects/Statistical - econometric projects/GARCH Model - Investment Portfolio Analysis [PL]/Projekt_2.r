###########################################
# Topic 5                                 # 
# Univariate GARCH model                  #
###########################################

rm(list = ls())
# setwd(.)

require(zoo)
require(xts)
require(moments)
require(forecast)
require(tseries)
require(rugarch)
require(knitr)
require(ggplot2)
require(copula)
require(psych)
require(MASS)
source("Block2Functions.R")

set.seed(10)
# 1. Loading data for WIG20 stocks
####################################
pkn <- read.csv(paste("https://stooq.pl/q/d/l/?s=","pkn","&i=d", sep = ""))
pkn <- zoo(pkn$Zamkniecie, as.Date(pkn$Data))

mbk <- read.csv(paste("https://stooq.pl/q/d/l/?s=","mbk","&i=d", sep = ""))
mbk <- zoo(mbk$Zamkniecie, as.Date(mbk$Data))

data <- merge(pkn, mbk)

dates     <- index(data) 
names(data)

# selecting the sample
startDate <- as.Date("2005-01-01")
endDate   <- as.Date("2050-01-01")
y         <- window(data, start=startDate, end=endDate)

# selecting a pair of stocks
y          <- na.omit(y[,c("pkn","mbk")])

# log returns of stocks
dy  <- 100*diff(log(y))

# porfolio weights
w   <- c(0.5,0.5)

# portfolio returns
r   <- zoo(dy%*%w,index(dy))
R   <- as.numeric(coredata(r))

# the value of investment in the portfolio
P   <- exp(cumsum(r/100))

par(mfrow=c(1,1), cex = 0.75, bty="l")
plot(P, col = "royalblue3", lwd = 2, main = "Poziom ceny", xlab = "Cena", ylab = "Data", 
     cex.main  = 1.5, cex.axis = 1.4, cex.lab = 1.4)

plot(r, main = "Log zwroty", col = "royalblue3", xlab = "Data", ylab = "Log zwroty",
     cex.main  = 1.5, cex.axis = 1.4, cex.lab = 1.4); abline(h=0)

# 2. Some statistics and charts
###################################

# summary statistics
Nyear <- 365/as.numeric(mean(diff(dates)))
mu    <- mean(r)*Nyear # srednia roczna stopa zwrotu
sig   <- sd(r)*sqrt(Nyear)  # odchylenie standardowe
mom <- as.data.frame(c(Nyear,mu,sig,min(r),max(r), skewness(r), kurtosis(r),jarque.bera.test(R)$stat)) # momenty
rownames(mom) <- c("N","mu","sig","min","max","skew","kurt", "JB test"); colnames(mom)="value"
kable(mom, digits=3)

# ACF plot
dev.off()
Acf(R, main = "ACF of daily returns", cex.main  = 1.5, cex.axis = 1.4, cex.lab = 1.4)
Box.test(R, lag = 20, type = c("Ljung-Box"))

# estymacja stopni swobody rozkladu t studenta
v = 4 + 6/(kurtosis(R)-3) # liczba stopni swobody w rozkladzie t studenta

# QQ plot
Rstar <- (R-mean(R))/sd(R)
q            <- seq(1/length(Rstar),1-1/length(Rstar), 1/length(Rstar))
Qteo         <- qnorm(q)                  # rozklad normalny
Qteo  <- qt(q,v)*sqrt((v-2)/v)            # for t-Student the variance is v/(v-2)
# Qteo  <- qdist("std",p=q,shape=5)

Qemp         <- quantile(Rstar,q)         # dane empiryczne

QQtab = data.frame(Qteo,Qemp)
ggplot(QQtab) + 
  geom_point(aes(x=Qteo, y=Qemp), size=2,shape=23, col="royalblue3")+
  theme_light()+
  labs(title = "Porownanie kwantyli empirycznych i teoretycznych (t-student v = 5)", 
       x = "Kwantyl teoretyczny", y = "Kwantyl empiryczny") +
  xlim(min(QQtab),max(QQtab))+ylim(min(QQtab),max(QQtab)) +
  geom_abline(intercept=0, slope=1, size=1) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        title = element_text(size = 18))


# 3a. VaR/ES for 1-steap ahead horizon: 
# Normal, t-Student, Historical Simulation
######################################################

# normal distribution 
p   <- 0.05      # tolerance level
m   <- mean(R) 
s   <- sd(R)
VaRnorm     <- qnorm(p)*s + m             
ESnorm      <- m - s*dnorm(qnorm(p))/p    
qf          <- function(x) qdist("norm", p=x)
ESnorm1     <- m + s*(1/p * integrate(qf, 0, p)$value) 

# t-Student distribution 
# v = 4 + 6/(kurtosis(R)-3)               # GMM estimate
# v <- fitdist("std", R)$pars["shape"]    # ML estimate
v = 5 #przyjmujemy 5 stopni swobody
VaRt  <- m + s*qdist("std",shape=v,p=p)
qf    <- function(x) qdist("std", p=x, shape=v)
ESt   <- m + s*(1/p * integrate(qf, 0, p)$value) 

# Historical simulation
R0    <- sort(R)              
N0    <- floor(length(R0)*p)                            
VaRhs <- R0[N0]                      # compare to: quantile(R,p)
EShs  <- mean(R0[1:N0])              

# 3b. VaR/ES for H-steap ahead horizon: 
# Normal, t-Student, Historical Simulation
######################################################

H = 10
data
# normal distribution: square root of time
VaRHnorm <- sqrt(1:H)*qnorm(p)*s + (1:H)*m    
ESHnorm  <- (1:H)*m - sqrt(1:H)*s*dnorm(qnorm(p))/p    

# t-distribution: Monte Carlo simulations
Nsim    <- 10000
Rdraws  <- matrix(rdist(distribution="std", Nsim*H, mu = m, sigma = s, shape = v),H,Nsim) 
RdrawsC <- apply(Rdraws,2,cumsum)               # cumualted changes
VaRHt   <- ESHt <- rep(NaN,H)
M0  <- floor(Nsim*p)     # observation for p-th quantile 
for(h in 1:H){
  temp   = sort(RdrawsC[h,])
  VaRHt[h] = temp[M0]
  ESHt[h]  = mean(temp[1:M0])
}

# Historical Simulation: Bootstrap
Rdraws  <- matrix(sample(R, Nsim*H, replace=TRUE),H,Nsim) 
temp   <- RdrawsToVaRES(RDraws,p)
VaRHhs <- temp$VaR
ESHhs  <- temp$ES

kable(data.frame(h=1:H,VaRHhs,VaRHnorm, VaRHt),digits=2)

# Volatility clustering
###########################################

# squared ACF
Acf(R^2, main="ACF of squared daily returns" )
Box.test(R^2, lag = 20, type = c("Ljung-Box"))

# 6. Selecting the specificatio of GARCH(p,q)  
#############################################
# dist = c("norm", "snorm", "std", "sstd")

LagSel <- function(x, Pmax=4, Qmax=4, crit="SIC", dist="norm"){
  IC <- matrix(NA, Pmax, Qmax+1)
  for(p in 1:Pmax){
    for(q in 0:Qmax){
      
      spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(p,q)), 
                        mean.model=list(armaOrder=c(0,0), include.mean=TRUE),  
                        distribution.model=dist)
      fit  = ugarchfit(data=x, spec=spec)
      if(crit == "AIC"){IC[p,q+1] <- infocriteria(fit)[1] }
      if(crit == "SIC"){IC[p,q+1] <- infocriteria(fit)[2] }
      if(crit == "HQ"){	IC[p,q+1] <- infocriteria(fit)[4] }
    }
  }
  rownames(IC) <- paste('p=',1:Pmax, sep="")
  colnames(IC) <- paste('q=',0:Qmax, sep="")
  return(IC)
}

ICtab <- LagSel(r,4,4,crit="SIC", dist="std")
kable(ICtab,digits=3)

# optimal lags
pq   = c(1,1)
PQ   = c(0,0)
dist = "std"

spec1 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=pq), 
                   mean.model=list(armaOrder=PQ, include.mean=TRUE),  
                   distribution.model=dist)
fit1 = ugarchfit(data=r, spec=spec1)
plot(fit1, which=12)


# 7. Extensions GARCH(1,1)  
######################################

# Leverage effect
Ccf(R^2,R, type="correlation", main="cross correlation of squarred returns and returns")

# assymetric models
spec.e   = ugarchspec(variance.model=list(model="eGARCH", garchOrder=pq), 
                      mean.model=list(armaOrder=PQ, include.mean=TRUE),  
                      distribution.model=dist)
spec.gjr = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=pq), 
                      mean.model=list(armaOrder=PQ, include.mean=TRUE),  
                      distribution.model=dist)

fit.e   = ugarchfit(data=r, spec=spec.e)    # solver="nlminb"
fit.gjr = ugarchfit(data=r, spec=spec.gjr)

# info criteria
IC <- cbind(infocriteria(fit1), infocriteria(fit.e), infocriteria(fit.gjr))
colnames(IC) <- c("GARCH", "eGARCH", "gjrGARCH")
kable(IC,digits=3)

mod = "gjrGARCH"

# GARCH-in-Mean #
spec.m = ugarchspec(variance.model=list(model=mod, garchOrder=pq), 
                    mean.model=list(armaOrder=PQ, include.mean=TRUE, archm = TRUE), distribution.model=dist)
fit.m  = ugarchfit(data=r, spec=spec.m)


IC <- cbind(infocriteria(fit1), infocriteria(fit.e), infocriteria(fit.gjr), infocriteria(fit.m))
colnames(IC) <- c("GARCH", "eGARCH", "gjrGARCH","GARCH-in-mean")
kable(IC,digits=3)

archM = TRUE

# best model
spec2 = ugarchspec(variance.model=list(model=mod, garchOrder=pq), 
                   mean.model=list(armaOrder=PQ, include.mean=TRUE, archm = archM), distribution.model=dist)
fit2  = ugarchfit(data=r, spec=spec2)


# 8. Analitical VaR/ES for h=1 from the best GARCH model
########################################################

p <- 0.05                        # tolerance level
v <- fit2@fit$coef["shape"]      # shape of t-distribution

# help(ugarchforecast)
fct2 <- ugarchforecast(fit2,data=r, n.ahead = 1)
sig <- sigma(fct2)
mu  <- fitted(fct2)

qf   <- function(x) qdist("std", p=x, shape=v)
#qf   <- function(x) qdist("norm", p=x)
VaRgarch <- mu + sig*qdist("std", p, shape=v)
ESgarch  <- mu + sig*(1/p * integrate(qf, 0, p)$value)  

# 9. Monte Carlo VAR/ES for longer horizons for the best GARCH model
####################################################################

H    = 10
Nsim = 10000
# help(ugarchsim)
sim2 <- ugarchsim(fit2, n.sim = H, n.start = 0, m.sim = Nsim, startMethod = "sample")
#names(sim1@simulation)
Rdraws  <- fitted(sim2)
temp   <- RdrawsToVaRES(RDraws,p)
VaRHgarch <- temp$VaR
ESHgarch <- temp$ES

# 10. Saving results for the next meetings
##############################################
VaRH <- cbind(VaRHnorm, VaRHt,VaRHhs,VaRHgarch)
ESH  <- cbind(ESHnorm, ESHt,ESHhs,ESHgarch)

kable(cbind(1:H,-VaRH),digits=2)
kable(cbind(1:H,-ESH),digits=2)

save("VaRH","ESH", file = "VaRESH.RData")

###########################################
# Topic 6                                 # 
# Multivariate GARCH model                #
###########################################

# 2. Some charts
#############################
# rolling moments

# to niepotrzebne raczej
plot(dy)

wd <- 52
Rsd1  <- rollapply(dy[,1], width=wd, sd, align="right")
Rsd2  <- rollapply(dy[,2], width=wd, sd, align="right")
Rcor  <- rollapply(dy, width=wd, function(x) cor(x[,1],x[,2]), by.column=FALSE, align="right")
Rcov  <- rollapply(dy, width=wd, function(x) cov(x[,1],x[,2]), by.column=FALSE, align="right")

#windows()
par(mfrow=c(2,2), cex=0.7, bty="l")
plot(Rsd1, main="Rolling SD for asset 1", xlab="", ylab="")
plot(Rsd2, main="Rolling SD for asset 2", xlab="", ylab="")
plot(Rcor, main="Rolling correlation", xlab="", ylab=""); abline(h=0)
plot(Rcov, main="Rolling covariance", xlab="", ylab=""); abline(h=0)

############################################################
# 3. Model GO-GARCH                                        #
# Generalized Orthogonal GARCH model                       #
# x_t  = Z y_t, y_t ~ N(0,H_t)                             #
# H_t  = diag(h_1t,...,h_nt)                               #
# h_it = w_i + a_i y_{i,t-1}^2 + b_i h_{i,t-1}             #
# Var(x_t) = ZH_tZ'                                 	     #
# E(H_t) = I, so that unconditional variance of y_t is ZZ' #
# Spectral decomposition of uncond. variance:              #
# VL^(0.5)U U'L^(0.5)V' = ZZ', U - ortonormal matrix       #
############################################################

require(rmgarch)

# help(gogarchspec)
mod  = "gjrGARCH" # "sGARCH", "iGARCH"
pq   = c(1,1)

specGO <- gogarchspec(mean.model        = list(model = "constant", external.regressors = NULL),
                      variance.model     = list(model = mod, garchOrder = pq), 
                      distribution.model = c("mvnorm"))

# help(gogarchfit)
fitGO <- gogarchfit(specGO, dy)  # solver = "solnp"

# coefficients
fitGO@mfit$arcoef
fitGO@mfit$garchcoef

# some matrices and time series
resX <- fitGO@mfit$residuals      # orignal series (residuals)
resY <- fitGO@mfit$Y              # unobserved factors
U    <- fitGO@mfit$U              # orthonormal matrix U%*%t(U)=I
L    <- fitGO@mfit$D              # eigenvalues of ZZ': eigen(var(resX))
V    <- fitGO@mfit$E              # eigenvectors
Z    <- V%*%L^0.5%*%U             # check resX - resY%*%t(Z) = 0

# conditional sd and cor
# plot of conditional sd, corr, cov
Ht     <- rcov(fitGO)
dates  <- as.Date(names(Ht[1,1,]))
GOsd1  <- zoo(Ht[1,1,]^0.5,order.by = dates)
GOsd2  <- zoo(Ht[2,2,]^0.5,order.by = dates)
GOcov  <- zoo(Ht[2,1,],order.by = dates)
Rt     <- rcor(fitGO)
GOcor  <- zoo(Rt[2,1,],order.by = dates)

#windows()
par(mfrow=c(2,2), cex=0.75, bty="l", mar=c(3,3,3,3))
plot(GOsd1, main="conditional SD, asset 1", xlab="", ylab=""); 
plot(GOsd2, main="conditional SD, asset 2", xlab="", ylab=""); 
plot(GOcor, main="conditional correlation", xlab="", ylab=""); abline(h=0)
plot(GOcov, main="conditional covariance", xlab="", ylab=""); 

# GO-GARCH forecast
#####################

# help(gogarchforecast)
H     <- 10
fctGO <- gogarchforecast(fitGO, n.ahead = H) 

# the values of our forecasts
fitted(fctGO)  
rcov(fctGO)    
rcor(fctGO)    

HFt      <- rcov(fctGO)
datesF   <- as.Date(names(HFt)) + seq(1:H)         # for simplicity I don't adjust for working days calendar (it is doable in bizdays)
GOsd1F   <- zoo(HFt[[1]][1,1,]^0.5,order.by = datesF)
GOsd2F   <- zoo(HFt[[1]][2,2,]^0.5,order.by = datesF)
GOcovF   <- zoo(HFt[[1]][2,1,],order.by = datesF)
RFt      <- rcor(fctGO)
GOcorF   <- zoo(RFt[[1]][2,1,],order.by = datesF)

# plot
B      <- 100
x_lim  <- c(datesF[1]-B,datesF[H])

#windows()
par(mfrow=c(2,2), cex=0.75, bty="l", mar=c(3,3,3,3))
plot(GOsd1[dates>datesF[1]-B], main="conditional SD, asset 1", xlab="", ylab="", xlim=x_lim); lines(GOsd1F, col="red")
plot(GOsd2[dates>datesF[1]-B], main="conditional SD, asset 2", xlab="", ylab="", xlim=x_lim); lines(GOsd2F, col="red")
plot(GOcor[dates>datesF[1]-B], main="conditional correlation", xlab="", ylab="", xlim=x_lim); lines(GOcorF, col="red"); abline(h=0)
plot(GOcov[dates>datesF[1]-B], main="conditional covariance", xlab="", ylab="", xlim=x_lim); lines(GOcovF, col="red"); abline(h=0)

# Simulations and VaR/ES for the portolio
###########################################

# help(gogarchsim)
H      <- 10      # horizon
Nsim   <- 10000   # number of draws
p      <- 0.01    # tolerance level
simGO  <- gogarchsim(fitGO, n.sim = H, m.sim = Nsim, startMethod = c("sample"))
help(gogarchsim)
draws  <- simGO@msim$seriesSim # draws for returns

Rdraws <- matrix(NA,H,Nsim) # return from investing in our portfolio of 2 assets
for (m in 1:Nsim){
  Rdraws[,m]  = draws[[m]]%*%w    # w should be a vector of weights
}

temp     <- RdrawsToVaRES(RDraws,p)
VaRHgo   <- temp$VaR
ESHgo    <- temp$ES
VaRHgo
ESHgo


###################################################################
# 4. DCC-GARCH                                                    #
#                                                                 #
# y_t = mu + e_t, e_t\sim N(0,\Sigma_t)                           #
# \Sigma_t = D_t*R_t*D_t,                                         # 
# e_t - D_t*z_t                                                   #
# R_t - conditional correlation matrix                            #
# D_t - diag(h_1t^0.5;...;h_Nt^0.5)                               #
# D_t - conditional variance                                      #
# h_t = a_0 + Ae_{t-1}.*e_{t-1} + Bh_{t-1}                        #
# R_t = (Q_t.* I)^{-0.5}Q_t(Q_t.* I)^{-0.5} scaling               #
# Q_t = (1-\alpha-\beta)Q + \alpha(z_{t-1}z_{t-1}' + \betaQ_{t-1} # 
###################################################################
require(rmgarch)

# help(dccspec)
# a. specification of univariate GARCH, separately for each individual asset

mod  = "gjrGARCH" # "sGARCH", "iGARCH"
pq   = c(1,1)
PQ   = c(0,0)
dist = "std"      # "norm"

specU = ugarchspec(variance.model=list(model=mod, garchOrder=pq), 
                   mean.model=list(armaOrder=PQ, include.mean=TRUE), 
                   distribution.model=dist)
mspec = multispec(c(specU, specU))


specDCC <- dccspec(mspec, dccOrder = c(1,1), model = "DCC")
fitDCC  <- dccfit(specDCC,dy)  # start.pars = list())
fitDCC

# some matrices and time series
Qbar  <- fitDCC@mfit$Qbar   
Qt    <- fitDCC@mfit$Q      # correlation matrix before scalling
Rt    <- fitDCC@mfit$R      # correlation matrix after scaling
Ht    <- fitDCC@mfit$H      # conditional covariance matrix

# compare average values of conditional covariance with unconditional covariance
apply(Ht,c(1,2),mean)
cov(dy)

# plot of conditional sd, corr, cov
Ht     <- rcov(fitDCC)
dates  <- as.Date(names(Ht[1,1,]))
DCCsd1  <- zoo(Ht[1,1,]^0.5,order.by = dates)
DCCsd2  <- zoo(Ht[2,2,]^0.5,order.by = dates)
DCCcov  <- zoo(Ht[2,1,],order.by = dates)
Rt     <- rcor(fitDCC)
DCCcor  <- zoo(Rt[2,1,],order.by = dates)


ylimC = c(min(GOcor,DCCcor)-0.01,max(GOcor,DCCcor)+0.01)
#windows()
par(mfrow=c(2,2), cex=0.75, bty="l",mar=c(3,3,3,3))
plot(DCCsd1, main="conditional SD, asset 1", xlab="", ylab=""); lines(GOsd1,col=2)
plot(DCCsd2, main="conditional SD, asset 2", xlab="", ylab=""); lines(GOsd2,col=2)
plot(DCCcor, main="conditional correlation", xlab="", ylab="",ylim=ylimC); abline(h=0); lines(GOcor,col=2)
plot(DCCcov, main="conditional covariance", xlab="", ylab=""); abline(h=0); lines(GOcov,col=2)

# Forecast
# help(dccforecast)
############################################

H      <- 10
fctDCC <- dccforecast(fitDCC, n.ahead = H) 

# the values of our forecasts
fitted(fctDCC)  # or fctDCC@mforecast$mu
rcov(fctDCC)    # or fctDCC@mforecast$H
rcor(fctDCC)    # or fctDCC@mforecast$R

HFt      <- rcov(fctDCC)
datesF   <- as.Date(names(HFt)) + seq(1:H)         # for simplicity I don't adjust for working days calendar
DCCsd1F  <- zoo(HFt[[1]][1,1,]^0.5,order.by = datesF)
DCCsd2F  <- zoo(HFt[[1]][2,2,]^0.5,order.by = datesF)
DCCcovF  <- zoo(HFt[[1]][2,1,],order.by = datesF)
RFt      <- rcor(fctDCC)
DCCcorF  <- zoo(RFt[[1]][2,1,],order.by = datesF)

# plot
B      <- 100
x_lim  <- c(datesF[1]-B,datesF[H])

#windows()
par(mfrow=c(2,2), cex=0.75, bty="l",mar=c(3,3,3,3))
plot(DCCsd1[dates>datesF[1]-B], main="conditional SD, asset 1", xlab="", ylab="", xlim=x_lim); lines(DCCsd1F, col="red")
plot(DCCsd2[dates>datesF[1]-B], main="conditional SD, asset 2", xlab="", ylab="", xlim=x_lim); lines(DCCsd2F, col="red")
plot(DCCcor[dates>datesF[1]-B], main="conditional correlation", xlab="", ylab="", xlim=x_lim); lines(DCCcorF, col="red"); abline(h=0)
plot(DCCcov[dates>datesF[1]-B], main="conditional covariance", xlab="", ylab="", xlim=x_lim); lines(DCCcovF, col="red"); abline(h=0)


# Simulations and VaR/ES for the portolio
###########################################

# help(dccsim)
H      <- 10      # horizon
p <- 0.01         # tolerance level for VaR/ES
Nsim   <- 10000   # number of draws
simDCC <- dccsim(fitDCC, n.sim = H, m.sim = Nsim, startMethod = c("sample"), rseed = 7)

draws  <- simDCC@msim$simX # draws for returns

Rdraws <- matrix(NA,H,Nsim) # return from investing in our portfolio of 2 assets
for (m in 1:Nsim){
  Rdraws[,m]  = draws[[m]]%*%w    # w should be a vector of weights
}

temp      <- RdrawsToVaRES(RDraws,p)
VaRHdcc   <- temp$VaR
ESHdcc    <- temp$ES
VaRHdcc
ESHdcc 

###########################################
# Topic 7                                 # 
# Copulas                                 # 
###########################################


# Introduction: create your own series with copulas and marginals                    
######################################################################

require(copula)
# tu na poczatku jakies rzeczy tylko do lepszego zrozumienia
# STEP 1. defining copula
Cop1 <- ellipCopula(family = "normal", dim = 2, param = 0.5)
Cop2 <- ellipCopula(family = "t", dim = 2, param = 0.5, df = 3)
Cop3 <- archmCopula(family = "clayton", param = 3) # skorelowane w momentach spadkowych
Cop4 <- archmCopula(family = "frank", param = 5)  # symetryczna po stronie spadkow jak i wzrostow
Cop5 <- archmCopula(family = "gumbel", param = 2) # skorelowane w momentach wzrostowych
Cop   <- Cop1

# random draws from copula, i.e. for (U,V)
tempUV <- rCopula(1000,Cop)
colnames(tempUV) <- c("U", "V")

# windows()
par(mfrow=c(2,2), cex = 0.7, bty="l", mar=c(3,3,3,3))
persp(Cop, pCopula, main = "copula: C(U,V)")
persp(Cop, dCopula, main = "copula density: c(U,V)")
contour(Cop, dCopula, main = "contour for c(U,V)")
plot(tempUV,pch=19, main = "Scatter plot: U vs. V")

# tu jest wazne
# Selecting the best copula
#####################################

# Step 1: selecting marginals
require(rugarch)
require(moments)

# wybieramy rozklady brzegowe t-studenta (jak Rubaszek)
mType <- 3

# Empirical marginal cdf 
if(mType==1){
  T  <- dim(dy)[1]
  UV <- matrix(0,T,2)
  UV[order(coredata(dy)[,1]),1] <- seq(1,T)/T;  
  UV[order(coredata(dy)[,2]),2] <- seq(1,T)/T;
}
# Normal distribution
if(mType==2){
  m1  <- mean(dy[,1]); s1 <- sd(dy[,1]); U  <- pdist("norm",(dy[,1]-m1)/s1); 
  m2  <- mean(dy[,2]); s2 <- sd(dy[,2]); V  <- pdist("norm",(dy[,2]-m2)/s2);
  UV  <- as.matrix(cbind(U,V))
}
# t-Student
if(mType==3){
  m1  <- mean(dy[,1]); s1 <- sd(dy[,1]); v1 <- 4 + 6/(kurtosis(dy[,1])-3); U  <- pdist("std",(dy[,1]-m1)/s1,shape = v1); 
  m2  <- mean(dy[,2]); s2 <- sd(dy[,2]); v2 <- 4 + 6/(kurtosis(dy[,2])-3); V  <- pdist("std",(dy[,2]-m2)/s2,shape = v2 );  
  UV  <- as.matrix(cbind(U,V))
}

par(mfrow=c(1,2), cex = 0.7, bty="l")
plot(coredata(dy),main = "Scatter plot for XY")
plot(UV, main = "Scatter plot for marginal distr.")


# Step 2. Fitting copula to UV
Cop1 <- normalCopula()
Cop2 <- tCopula(dispstr = "un")
Cop3 <- gumbelCopula()
Cop4 <- claytonCopula()
Cop5 <- frankCopula()
Cop  <- Cop5 # po kolei podstawiac inna copule i patrzec na loglikelihood


# CopEst <- fitCopula(Cop, u, method = "itau")
sVal <- fitCopula(Cop, UV, method = "itau")@estimate # oszacowanie korelacji
CopEst <- fitCopula(Cop, UV, method = "ml", start = sVal) # oszacowanie funckji laczacej

# Kopula dla t studenta
CopEst <- fitCopula(Cop2, UV, method = "itau.mpl", start=c(cor(dy)[1,2],5))
CopEst <- fitCopula(Cop2, UV, method = "ml", start=c(cor(dy)[1,2],5))

CopEst@estimate
CopEst@loglik

# najlepsza kopula dla t studenta - najwyzsze log likelihood

# 8. VaR/ES
#####################################
H    = 10
Nsim = 10000
p    = 0.05     # tolerance level

# a. Copula
UVsim <- rCopula(Nsim*H,CopEst@copula)

# b. returning to obsevables
# Empirical marginal cdf 
if(mType==1){XYsim <- cbind(quantile(dy[,1],UVsim[,1]), quantile(dy[,2],UVsim[,2]))}
# Normal distribution
if(mType==2){XYsim <- cbind(qdist("norm",UVsim[,1])*s1+m1, qdist("norm",UVsim[,2])*s2+m2)}
# t-Student
if(mType==3){XYsim <- cbind(qdist("std",UVsim[,1],shape = v1)*s1+m1, qdist("std",UVsim[,2],shape = v2)*s2+m2)}

par(mfrow=c(1,2), cex = 0.7, bty = "l")
plot(coredata(dy),  main = 'Returns', xlim = c(-10, 10), ylim = c(-20, 20), 
     cex.main  = 1.5, cex.axis = 1.4, cex.lab = 1.4)
plot(XYsim[1:dim(dy)[1],], col = 'red', main = 'Simulated', xlim = c(-10, 10), ylim = c(-20, 20), 
     cex.main  = 1.5, cex.axis = 1.4, cex.lab = 1.4)

# portfolio return
Rdraws     <- matrix(XYsim%*%w,H,Nsim)
temp       <- RdrawsToVaRES(RDraws,p)
VaRHcop    <- temp$VaR
ESHcop     <- temp$ES

VaRH <- cbind(VaRH, VaRHgo, VaRHdcc, VaRHcop)
ESH  <- cbind(ESH,ESHgo, ESHdcc, ESHcop)

save("VaRH","ESH", file = "VaRESH.RData")

kable(cbind(1:H,-VaRH), digits=2)
kable(cbind(1:H,-ESH), digits=2)

# Backtesting             

# 2. Rolling VaR/ES 
#######################################

# Normal distribution / vs t-Distribution / vs historical simulations / vs EWMA

p     <- 0.05         # VaR/ES tolerance level
M     <- 250          # number of observations for backtesting
R     <- 500          # rolling window length (here we apply rolling forecasting scheme)


temp     <- NORMfct(r, M, R, p)
ESrnorm  <- temp$ES
VaRrnorm <- temp$VaR

temp   <- HSfct(r, M, R, p)
ESrhs  <- temp$ES
VaRrhs <- temp$VaR

temp   <- Tfct(r, M, R, p, v=5)
ESrt   <- temp$ES
VaRrt  <- temp$VaR

temp      <- EWMAfct(r, M, R, p, lambda=0.94)
ESrewma   <- temp$ES
VaRrewma  <- temp$VaR
ESrewma
# tutaj wybieramy model do testowania (spradzamy kazdy po kolei i zapisujemy w tabelce wyniki testow: 
# pvalue, actual i expected exceeds)

realized <- tail(r,M)
VaR      <- VaRrt
ES       <- ESrt

# graph in rugarch - mysle zeby dodac ten wykres dla kazdego modelu
VaRplot(p, actual=realized, VaR=VaR)
abline(h=0)
lines(ES,  col="blue",lwd=3)

###################
### Backtesting ###
###################

# expected number of exceedances - tam gdzie najwieksze prawdopodobienstwo
require(knitr)
K = 25
x = data.frame(n = 1:K,pdf=dbinom(1:K,size=M,prob=p),cdf=pbinom(1:K,size=M,prob=p))
kable(x,digits=3)

# actual number of exceedances
HR     <- as.numeric(realized < VaR) # hit ratio: VaR exceedances
a      <- mean(HR)

sum(HR) # faktyczna liczba przekroczen

# a. Unconditional coverage: Kupiec test
# H0: alpha = p
# alpha - expected value of VaR exceedance share
# see: https://www.value-at-risk.net/backtesting-coverage-tests/
####################################################################

# Kupiec test (tu liczenie reczne)
n  = length(HR)
n1 = sum(HR)
n0 = n - n1

LR_uc   = (p/a)^n1 *((1-p)/(1-a))^n0
stat_uc = -2*log(LR_uc)
prob_uc = 1 - pchisq(stat_uc,1, lower.tail=TRUE)
prob_uc # pvalue

# Kupiec test - The same in rugarch package 
temp <- VaRTest(alpha=p, coredata(realized), coredata(VaR))

# tu jest wszystko co potrzeba (expected exceed, actual exceed, pvalue (uc.LRp))
temp$expected.exceed # oczekiwana liczba przekroczec
temp$actual.exceed   # faktyczna liczba przekroczen
temp$uc.LRp          # pvalue
temp$uc.Decision     # decyzja

# b. Christofersen independence test) 
# H0: p(HR[t]=1|HR[t-1]=1) = p(HR[t]=1|HR[t-1]=0)      
###########################################################

# regression approach
# HR[t] = alpha + rho HR[t-1] + epsilon[t]
HR0 <- HR[2:M]
HR1 <- HR[1:(M-1)]

HR_reg = lm(HR0~HR1)
summary(HR_reg)
# H0: rho = 0
linearHypothesis(HR_reg, hypothesis.matrix=c(0,1), rhs=0, test="Chisq")

# Christofersen independence test
n00 = sum(!HR1 & !HR0) # no exceedance after no exceedance
n01 = sum(!HR1 & HR0)  # exceedance after no exceedance
n10 = sum(HR1 & !HR0)  # no exceedance after exceedance
n11 = sum(HR1 & HR0)   # exceedance after exceedance

n0  = n00 + n10
n1  = n01 + n11

pi01 = n01 / (n00+n01) # prob. of exceedance after no exceedance 
pi11 = n11 / (n10+n11) # prob. of exceedance after exceedance 
pi   = (n01+n11) / (n00+n01+n10+n11) # podobna wartosc do alpha

LR_ind   = pi^n1*(1-pi)^n0 / (pi01^n01 * pi11^n11 * (1-pi01)^n00  * (1-pi11)^n10)
stat_ind = -2*log(LR_ind)
prob_ind = 1 - pchisq(stat_ind,1)
prob_ind # p value


# c. Christofersen conditional coverage test                       
# H0: p(HR[t]=1|HR[t-1]=1) = p(HR[t]=1|HR[t-1]=0)=p      
#################################################################

# regression approach
HR_reg = lm(HR0~HR1)
summary(HR_reg)
# H0: alpha = ap, rho = 0
linearHypothesis(HR_reg, diag(1,2), c(p,0), "Chisq")

# Christofersen conditional coverage test
pi01 = n01 / (n00+n01) # prob. of exceedance after no exceedance 
pi11 = n11 / (n10+n11) # prob. of exceedance after exceedance 

# LR_cc   = (p/pi01)^n01 * (p/pi11)^n11 * ((1-p)/(1-pi01))^n00 * ((1-p)/(1-pi11))^n10
LR_cc=LR_ind*LR_uc
stat_cc = -2*log(LR_cc)
prob_cc = 1 - pchisq(stat_cc,2)
prob_cc # p value


# d. McNeil and Frey test for well calibrated Expected Shartfall  
################################################

# only when VaR was exceeded
ind <- which(HR==1)
mean(VaR[ind])
mean(ES[ind])
mean(realized[ind])

temp      <- realized[ind]-ES[ind]
m         <- mean(temp)
s         <- sd(temp)
N         <- length(temp)
stat_mf   <- mean(temp)/ (sd(temp)/sqrt(N))
prob_mf   <- 1- pnorm(abs(stat_mf))
prob_mf # pvalue

