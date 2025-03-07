rm(list = ls())
require(ggplot2)
require(knitr)
require(forecast)
require(urca)
require(quantmod)
require(zoo)
require(xts)
require(lubridate)
require(grid)
require(gridExtra)
require(tidyr)
require(kable)
require(vars)
require(eurostat)
require(tseries)
source("Block 1/Block1codes/Block1Functions.R")

#########################################################
### 1. Pobranie danych

# Szwecja inflacja
data <- get_eurostat("prc_hicp_manr",
                     filters = list(geo    = "SE", 
                                    coicop = "CP00",
                                    unit   = "RCH_A"
                     ))

piSE <- na.omit(zoo(data$values, data$time))

##########################################################
### 2. Wykresy podstawowe

# Time series plot
plot(piSE,
     main = "Poziom inflacji w Szwecji (1997 - 2020)",
     xlab = "Rok",
     ylab = "Poziom inflacji (w %)",
     col = "aquamarine3",
     lw = 2)
abline(2, 0, lty = "longdash")
legend("topleft",
       legend = c("Poziom inflacji", "Cel inflacyjny"),
       col = c("aquamarine3", "black"),
       lwd = c(1.1,1.1), 
       lty = c(1,2),
       cex = 0.7,
       bty = "n")

# ACF plot
piSE_ts <- ts(data$values)

acf(piSE_ts, main = "Inflacja w Szwecji (ACF)", col = "aquamarine3", lwd = 2)

###########################################################
### 3. Test pierwiastka jednostkowego - UR (Unit root) tests

# Test ADF
summary(ur.df(piSE_ts, type="drift", lags=1)) # Odrzucamy H0
summary(ur.df(diff(piSE), type="drift", lags=1)) #Odrzucamy H0

# Test PP
summary(ur.pp(piSE, type = "Z-tau", model="constant"))

# Test KPSS
summary(ur.kpss(piSE, type = "mu"))

############################################################
### 4. Model ARMA

piSE_ts <- ts(coredata(piSE), frequency=12,start=c(year(piSE[1]),month(piSE[1])))

# Oszacowanie parametrow modelu (2,0,2)
arma.CSS_ML <- Arima(piSE_ts, order = c(2, 0, 2), method = "CSS-ML") # Mixed - defaultowy
arma.CSS    <- Arima(piSE_ts, order = c(2, 0, 2), method = "CSS") # Conditional sum of squares (MNK)
arma.ML     <- Arima(piSE_ts, order = c(2, 0, 2), method = "ML") # Maximum Likelihood

ARMA.coef <- matrix(NA,3,5)
ARMA.coef[1,] <- arma.CSS_ML[["coef"]]
ARMA.coef[2,] <- arma.CSS[["coef"]]
ARMA.coef[3,] <- arma.ML[["coef"]]
rownames(ARMA.coef) <- c("CSS-ML","CSS","ML")
colnames(ARMA.coef) <- c("ar1", "ar2", "ma1", "ma2", "cons.")

kable(ARMA.coef, digits = 2)

# selecting specification, inormation criteria - wybor specyfikacji modelu

LagSel <- function(x, Pmax=3, Qmax=3, d=0, crit="BIC"){
  IC <- matrix(NA, Pmax+1, Qmax+1)
  for(p in 0:Pmax){
    if(p==Pmax){x0=x} else {x0 = x[-(1:(Pmax-p))]} 
    for(q in 0:Qmax){
      if(crit == "AIC"){ IC[p+1,q+1] <- AIC(Arima(x0,order=c(p,d,q)), k=2) }
      if(crit == "BIC"){ IC[p+1,q+1] <- AIC(Arima(x0,order=c(p,d,q)), k=log(length(x)-p)) }
      if(crit == "HQ"){  IC[p+1,q+1] <- AIC(Arima(x[- (1:(Pmax-p+1))],order=c(p,d,q)), k=2*log(log(length(x)-p))) }
    }}
  rownames(IC) <- paste('ar',0:Pmax, sep=""); colnames(IC) <- paste('ma',0:Qmax, sep="")
  return(IC)
}

# Kryterium BIC - (1,0) lub (3,2)
kable(LagSel(piSE_ts, crit = "BIC"), digits = 2) 

# Kryterium AIC - (3,2)
kable(LagSel(piSE_ts, crit = "AIC"), digits = 2) 

arma0 <- arima(piSE_ts, order = c(1,0,0))  # parsimonous model
arma1 <- arima(piSE_ts, order = c(3,0,2))  # larger model

# Likelihood ratio test
LLtest <- function(arimaS, arimaL){
  d  <-  -2*(arimaS[["loglik"]] - arimaL[["loglik"]])
  df <-  length(arimaL[["coef"]]) - length(arimaS[["coef"]])
  p  <- 1 - pchisq(d, df)
  return(list(stat=d, prob = p)) 
}

as.data.frame(LLtest(arma0, arma1))

# Portmanteau test for autocorrelation - brak autokorelacji -> specyfikacja poprawna (1,0)
dev.off()
Acf(arma0$residuals, main = "ACF of ARMA (1,0) residuals", col = "aquamarine3", lwd = 2)

k <- length(arma0[["coef"]])-1
Box.test(residuals(arma0), lag = 4, type = "Ljung-Box", fitdf = k)

Jmax <- 12
LB   <- matrix(NA,Jmax-k, 2)
for(h in (k+1):Jmax){
  z <- Box.test(residuals(arma0), lag = h, type = "Ljung-Box", fitdf = k)
  LB[h-k,] <- c(z[["statistic"]], z[["p.value"]])
}
rownames(LB) <- paste('h=',(k+1):Jmax, sep=""); colnames(LB) <- c("LB stat.", "prawd.")
kable(LB, digits=3)

# JB normality test
jarque.bera.test(residuals(arma0))

e         <- residuals(arma0)/sd(residuals(arma0))
bwdth     <- 0.1
ggplot(data.frame(e), aes(x = e)) +
  theme_light() +
  geom_histogram(binwidth = bwdth, colour = "white", fill = "blue", size = 0.1) +
  stat_function(fun = function(x) dnorm(x)*length(e)*bwdth, color = "red", size = 2) +
  labs(title = "Empirical historgram vs normal distribution",
       y = "", x = "Standarized residuals", caption = "")

# Characteristic roots and MA representation 
summary(arma0)
# intercept = mu
# a. characteristic roots
ar.coef <- arma0[["coef"]][1]

#ma.coef <- arma0[["coef"]][4:5]
ma.coef = NULL

roots = polyroot(c(1, -ar.coef))
1/roots

plot(arma0)

# b. MA representation

# ARMA (1,0) IRF (Funckaj reakcji na impuls)
Kmax = 35
IRF <- ARMAtoMA(ar = ar.coef, ma = ma.coef, Kmax); IRF = c(1, IRF)
IRFtab <- data.frame(H = 0:Kmax, IRF = IRF)

MyPlot(IRFtab, main = "IRF from ARMA (1,0) for Sweden")

############################################################
### 6. Prognoza ARMA (1,0)

fcst <- forecast(arma0, h = 60)
fcst_arma <- forecast(arma0, h = 60)
head(fcst)
plot(fcst, include = 120, shaded = T, shadecols=c('grey','GhostWhite'), 
     main = "CPI inlation forecast", ylab = "", xlab = "", bty = "l")
abline(h = mean(piSE), col = "red")

# Annual forecasts
datesf <- last(index(piSE)) + months(1:60)
temp  <- zoo(fcst$mean, datesf)
xf = c(piSE, temp)
kable(apply.yearly(xf, mean))

arma_fct <- apply.yearly(xf, mean)

#############################################################
### 7. Model VAR

# Pobranie danych o inflacji z Szwecji i UE
temp <- get_eurostat("prc_hicp_manr",
                     filters = list(geo=c("EA","SE"),coicop="CP00",unit="RCH_A"))
tempF <- subset(temp, geo == "EA")
tempH <- subset(temp, geo == "SE")
pF  <- na.omit(zoo(tempF$values,order.by=tempF$time))
pH  <- na.omit(zoo(tempH$values,order.by=tempH$time))

# Wykres inflacji
z <- merge(pF, pH, all = FALSE)
y  <- ts(coredata(z), frequency = 12, start = c(year(z[1]), month(z[1])))

plot(z, screens=c(1,1), col = c("black", "aquamarine3"), lw = 2,
     main = "Inflacja w Szwecji i UE", ylab = "Poziom inflacji (w %)", xlab= "Rok", 
     type = "l", bty="l")
legend("topleft",legend=c("EA","SE"), col = c("black", "aquamarine3"), lty = 1, lwd = 1.1, bty = "n")


# VAR model
VARselect(z, lag.max = 8, type = "const") # wybor opoznienia - VAR(2)
p = 2
VARE <- VAR(z, p = p, type = "const")

# Testing for autocorrelation: test Ljunga-Boxa - H1
serial.test(VARE, lags.pt = 6, type="PT.adjusted")

par(mfrow = c(2,2), cex = 0.7, bty = "l", mar = c(3,3,3,3))
Acf(residuals(VARE)[,1], xlab = "", main = "e1 vs lags of e1")
Ccf(residuals(VARE)[,1], residuals(VARE)[,2], xlab = "", main = "e1 vs lags of e2", xlim = c(0,24))
Acf(residuals(VARE)[,2], xlab = "", main = "e2 vs lags of e2")
Ccf(residuals(VARE)[,2], residuals(VARE)[,1], xlab = "", main = "e2 vs lags of e1", xlim = c(0,24))

# Recursive structuralization

# causality
causality(VARE, cause = "pF")
causality(VARE, cause = "pH")

# recursive structuralization 
bmat <- matrix(0,2,2)
bmat[lower.tri(bmat, diag=T)] <- NA
SVARE <- SVAR(VARE, Amat = NULL, Bmat = bmat)
SVARE
B <- SVARE$B

# Structural IRF
K    <- 48
SVMA <- Phi(SVARE, nstep = K)

# short run-impact matrix
SVMA[,,1]

# Plot in Vars package
SIRF <- irf(SVARE, n.ahead=K, cum = F, boot = F)
plot(SIRF)

# Single IRF
IRFtab = data.frame(H = 0:K, IRF = SVMA[1,1,])
MyPlot(IRFtab, main= "IRF from uEA to infEA")

# Panel of IRFs  
require(grid)
require(gridExtra)
IRFtab$IRF = SVMA[1,1,]; irf11 <- MyPlot(IRFtab, main = "IRF from uEA to infEA")
IRFtab$IRF = SVMA[1,2,]; irf12 <- MyPlot(IRFtab, main = "IRF from uSE to infEA")
IRFtab$IRF = SVMA[2,1,]; irf21 <- MyPlot(IRFtab, main = "IRF from uEA to infSE")
IRFtab$IRF = SVMA[2,2,]; irf22 <- MyPlot(IRFtab, main = "IRF from uSE to infSE")
grid.arrange(irf11, irf12, irf21, irf22, ncol=2)

# FEVD
i      = 2            # which variables
SIRFi  = SVMA[i,,]    # IRF for variable i
temp   = SIRFi^2      # contribution of shock[t] to forecast variance at [t+h]
FVARi  = apply(temp,1,cumsum) # contribution of shocks[t:t+h] to forecast variance at [t+h]
FEVD0  <- prop.table(FVARi,1); colnames(FEVD0) = c("uEA","uSE")
FEVD1  <- data.frame(H = 0:K, eEA = FEVD0[,1], eSE = FEVD0[,2])

kable(FEVD1[c(1:5,13,25,49),],digits=3,row.names = FALSE)

require(tidyr)
FEVD2 <- gather(data = FEVD1, key = shocks, value = value, -c(H))
ggplot(FEVD2, aes(fill=shocks, y=value, x=H)) + 
  geom_bar( stat="identity") +   
  theme_light()+
  labs(title="FEVD for inflation in Sweden", y="", x="", caption="")+
  scale_fill_manual(values=c("grey70", "grey30"))

# function in the package
temp <- fevd(SVARE,n.ahead=60)
temp$pH
plot(temp)


# 6. Historical decomposition   #
#################################

i      = 2            # which variables
# a. Structural shocks
e <- residuals(VARE)
B <- SVARE$B
u <- t(solve(B)%*%t(e))
T <- dim(u)[1]
cor(u) 

# b. IRFs
SVMA <- Phi(SVARE, nstep=T)
SIRF  = t(SVMA[i,,])    # IRF for variable i

# c. Historical decompositoion
HistDec             <- matrix(NA,T,2)
colnames(HistDec)   <- c("uEA","uSE")

for(t in 1:T){
  junk1 <- as.matrix(u[1:t,])
  junk2 <- as.matrix(SIRF[t:1,])
  HistDec[t,] <- colSums(junk1*junk2)
}

# the impact of initial conditions
InitCond <- tail(coredata(z[,i]),T) - colSums(t(HistDec)) 
mu       <- as.numeric(tail(InitCond,1))
InitCond <- InitCond - mu
HistDec  <- cbind(HistDec, InitCond)

HD1   <- as.data.frame(HistDec); HD1$t   <- tail(index(z[,i]),T)
HD2   <- gather(data = HD1, key = shocks, value = value, -c(t))
Y     <- data.frame(y = tail(coredata(z[,i]),T)-mu,t=tail(index(z[,i]),T))

ggplot() + 
  geom_bar(data = HD2, aes(fill = shocks, y = value, x = t), stat = "identity", width = 50) + 
  scale_fill_manual(values=c("grey80", "firebrick2", "springgreen2"))+
  geom_line(data = Y, aes(y = y, x = t), size = 0.9)+
  theme_light()+
  labs(title = "Historical decomposition for inflation in Sweden", x = "Rok", y = "")

# 7. A forecast #
#################

H   <- 60
T   <- dim(z)[1]

fct <- predict(VARE, n.ahead=H)
fct
fanchart(fct, nc=1, xlim=c(T-60,T+H),bty="l",mar=c(3,3,1,1))

# fan chart in ggplot
datesf <- last(index(z))+months(1:H)
f      <- zoo(fct$fcst$pH[,1],datesf)

i = 2                                                       # select variable
inf_data  = data.frame(t=index(z),y = coredata(z[,i]))
inf_fct   = data.frame(t=datesf  ,y = fct$fcst[[i]][,1]) 
point_fct = rbind(inf_data,inf_fct)
interval_fct = data.frame(t=datesf,lower = fct$fcst[[i]][,2],upper = fct$fcst[[i]][,3]) 
MyPlot(point_fct, main = "Prognoza inflacji VAR(2) dla Szwecji", 
       xlab = "Rok", ylab = "Poziom inflacji (w %)", 
       hline = mean(z[,i])) +
  geom_ribbon(data= interval_fct, aes(x = t, ymin = lower, ymax = upper), alpha = 0.3, fill = "aquamarine3") +
  xlim(as.Date("2010-01-01"),as.Date("2023-01-01"))

fcst_var <- point_fct

# Annual forecasts
require(xts)
require(lubridate)
datesf <- last(index(z))+months(1:60)
temp     <- zoo(fct$fcst$pH[,1],datesf)
xf = c(z[,2],temp)

kable(apply.yearly(xf,mean),digits=2)

var_fct <- apply.yearly(xf,mean)

# Forecasting                           #
#########################################
# Calculating ex-post forecast
#####################

# settings
T          <- dim(z)[1] 	    # number of observations
M          <- 60              # Forecast evaluluation sample size
T1         <- T - M           # sample split (1st fct for T1+1)
horiz      <- 12              # forecast horizon
varN       <- 2               # which variable 

# Calculating forecasts and realizations
#########################################

act      <- Actfct(z[,varN], horiz, T1)
fctRW    <- RWfct( z[,varN], horiz, T1)
fctARMA  <- ARIMAfct(z[,varN], horiz, T1, 1, 0, 0)
fctVAR   <- VARfct(z, horiz, T1, varN, 2)

# 3. Sequential forecast
#####################################

# Choose model "RW", "ARIMA", VAR"
fct   = fctARMA

# make a graph
datesF = c(index(z),last(index(z)) + months(1:horiz))
inf_data    = data.frame(t=index(z[,varN]),y=coredata(z[,varN]))
temp_plot   <- MyPlot(inf_data, main = "Sequential forecast ARMA (1, 0)", xlab = "", hline = mean(z[,varN]))
for (n in 0:(T-T1-1)) {
  fct_data = data.frame(yf = fct[n+1,], tf = datesF[(T1+n+1):(T1+n+horiz)])
  temp_plot <- temp_plot + geom_line(data=fct_data, aes(y=yf,x=tf), colour="red", size=1)
}
temp_plot + 
  xlim(as.Date("2010-01-01"),as.Date("2022-01-01"))

# 4. Effectiveness test
###################################

h     = 12                          # Horizon
fct   = fctVAR                      # Choose model "RW", "ARIMA", "VAR"

ylevel  <- act[1:(T-T1-h+1),h] 
xlevel  <- fct[1:(T-T1-h+1),h] 
ychange <- act[1:(T-T1-h+1),h] - z[T1:(T-h),varN]
xchange <- fct[1:(T-T1-h+1),h] - z[T1:(T-h),varN]

par(mfrow=c(1,2), cex = 0.7, bty="l")
a1 <- min(cbind(xlevel,ylevel))-0.01; a2 <- max(cbind(xlevel,ylevel))+0.01; 
plot(xlevel,ylevel, main="level", xlab="forecast", ylab="realization", pch=19, col="red",  xlim=c(a1,a2), ylim = c(a1,a2))
abline(0,1,lwd=2)
a1 <- min(cbind(xchange,ychange))-0.01; a2 <- max(cbind(xchange,ychange))+0.01; 
plot(xchange,ychange, main="change", xlab="foreast", ylab="realization", pch=19, col = "blue", xlim=c(a1,a2), ylim = c(a1,a2))
abline(0,1,lwd=2)

fit <- lm(ylevel ~ xlevel)
summary(fit)
fit <- lm(ychange ~ xchange)
summary(fit)

require(car)
linearHypothesis(fit, hypothesis.matrix = diag(1,2), 
                 rhs=c(0,1), test="F") 


# 5. Table with statistics
##############################

errRW     <- act - fctRW 
errARMA   <- act - fctARMA  
errVAR    <- act - fctVAR 

statRW      <- FctErrStats(errRW)
statARMA    <- FctErrStats(errARMA)
statVAR     <- FctErrStats(errVAR)

dmRW       <- rep(NA,h)
dmARMA     <- DMstat(errARMA, errRW, type="two.sided", power=2)
dmVAR      <- DMstat(errVAR, errRW, type="two.sided", power=2)

ME   <- rbind(statRW$ME,statARMA$ME,statVAR$ME )
RMSE <- rbind(statRW$RMSE,statARMA$RMSE,statVAR$RMSE)
DM   <- rbind(dmRW,dmARMA,dmVAR)

models <- c("RW", "ARIMA", "VAR")
rownames(ME)   <- models; colnames(ME)   <- paste(1:horiz)
rownames(RMSE) <- models; colnames(RMSE) <- paste(1:horiz)
rownames(DM)   <- models; colnames(DM)   <- paste(1:horiz)

Hsel = c(1,2,3,6,9,12)


options(digits = 3)
kable(ME[,Hsel])
kable(RMSE[,Hsel])
kable(DM[,Hsel])

RMSE1 = RMSE / matrix(rep(RMSE[1,],3),3,12, byrow = TRUE)
RMSE1[1,] = RMSE[1,]
RMSE1[,Hsel]

# Diebold-Mariano test 

h   = 1
e1h = errRW[,h]
e2h = errVAR[,h]
dh  = na.omit(e1h^2 - e2h^2)
plot.ts(dh)
abline(h=0,col=2)
abline(h=mean(dh),col=3)

dhat = mean(dh)
s    = sd(dh)
Th   = length(dh)

require(sandwich)
sig2lr = lrvar(dh, type = "Newey-West")
sig2   = var(dh)/Th

DM = dhat/sqrt(sig2lr)
DM
pnorm(-abs(DM))
