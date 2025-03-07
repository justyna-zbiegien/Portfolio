#load packages
if(!require("readxl")) {install.packages("readxl"); library(readxl)}
if(!require("stargazer")) {install.packages("stargazer"); library(stargazer)}
if(!require("car")) {install.packages("car"); library(car)}
if(!require("manipulate")) {install.packages("manipulate"); library(manipulate)}
if(!require("mvtnorm")) {install.packages("mvtnorm"); library(mvtnorm)}
if(!require("tseries")) {install.packages("tseries"); library(tseries)}

# clear the workspace, plots and console
rm(list = ls()); if(!is.null(dev.list())) dev.off(); cat("\014") 

#set working directory
directory.main <- dirname(rstudioapi::getSourceEditorContext()$path)
directory.data <- paste(directory.main,"data",sep="/")
directory.functions <- paste(directory.main,"functions",sep="/")

setwd(directory.main)

#load data
race.laptimes <- readRDS("data.race.laptimes")
race.laptimes$lap_time.relative <- race.laptimes$lap_time.relative*100
#race.laptimes$posteriori$lap_time.relative <- race.laptimes$posteriori$lap_time.relative*100
race.laptimes$race.completed <- race.laptimes$race.completed*100
#race.laptimes$posteriori$race.completed <- race.laptimes$posteriori$race.completed*100

#add interactions to df
#for (i in c("priori","posteriori")) {for (j in c('tyre_softer','tyre_medium','tyre_harder')){eval(parse(text = paste("race.laptimes$",i,"$tyre_age.",j,"<- race.laptimes$",i,"$tyre_age * race.laptimes$",i,"$",j, sep="")))}}
for (j in c('tyre_softer','tyre_medium','tyre_harder')){eval(parse(text = paste("race.laptimes$","tyre_age.",j,"<- race.laptimes$tyre_age * race.laptimes$",j, sep="")))}

#1. Regresja pomocnicza (OLS) w celu uzyskania oszacowan a priori
OLS_results <- lm(lap_time.relative~tyre_age
                   #+tyre_age.sq
                   +race.completed + tyre_medium +
                     tyre_softer + tyre_age.tyre_medium + tyre_age.tyre_softer + Main_driver
                   ,data=race.laptimes)
summary(OLS_results)

VIF(OLS_results)
hist(race.laptimes$tyre_age)
res <- OLS_results$residuals
plot(res)
jarque.bera.test(res)
hist(res,main="Histogram reszt z regresji OLS",xlab="Reszty",ylab="Czêstoœæ")

aux.names <- names(OLS_results$coefficients)
for (i in aux.names[!aux.names %in% c("(Intercept)","tyre_medium","tyre_softer","Main_driver","tyre_age.tyre_medium","tyre_age.tyre_softer")]){
  eval(parse(text = paste("hist(race.laptimes$",i,", main='Histogram of ",i,"')",sep="")))
  eval(parse(text = paste("plot(density(race.laptimes$",i,"), main='Density of ",i,"')",sep="")))
}

# prior expectations
# Intercept - Certainly positive coefficient & we can vaguely assume a weak prior, e.g. normal distribution with expected value being ca.
#             track record time, but assuming that race pace will be roughly 10% slower than quali
# tyre_age - expect slower laps as the length of the stint increases due to tyre wear
# race.completed - difficult to tell as within this variable several factors could be conceived
#                  Nevertheless, decreasing fuel load should have an overwhelming impact, therefore negative coefficient seems reasonable
# tyre_medium, tyre_softer - under normal circumstances, softer tyre (baseline is 'hard') provides drivers with greater
#                            grip and in general should result in faster laps (at least until the tyre 
#                            reaches its' life expectancy, at which point decay is expected due to excessive wear)
#                            Broadly we should expect a negative sign for both, and probably a smaller one for the softest tyre
# tyre_age.tyre_medium, tyre_age.tyre_softer - supposed to capture the decay in tyres' performance as the stint length increases
#                                              positive sign to be expected
# Main_driver - difficult to tell in advance, though I suspect that main drivers are chosen teams' leading drivers
#               for a reason, hence I'd expect a negative coefficient a posteriori. Of course, this should be evaluated over the course
#               of the whole season, as every driver has better/worse races. In this case we will assume weak priors
#               and let the data speak for itself in case of this particular GP.

c_names <- names(summary(OLS_results)$coefficients[,2])

# Uzasadnienie parametrów a priori
# tyre_softer - 0.6-0.8 second per lap faster than than medium tyre --> 1.2-1.6 second per lap faster than hard tyre
# tyre_medium - 0.6-0.8 second per lap faster than hard tyre
# tyre_age: https://f1metrics.wordpress.com/2017/03/14/2017-preseason-analysis/
# tyre_age - 0.07 second per lap
# tyre_age*compound impact has to be deduced by accounting for the fact that hard tyre serves as the basis 
# tyre_age*tyre_softer - 0.11 second per lap; - (random guess) 0.04 second per lap (hard tyre)
# tyre_age*tyre_medium - 0.07 second per lap; - (random guess) 0.04 second per lap (hard tyre)

hard_compound_deter_basis = 0.04 # guessing the deterioration rate of a given tyre per lap
medium_compound_deter = 0.07
soft_compound_deter = 0.11

expected_values <- data.frame(matrix(data=NA,nrow=8,ncol=2),row.names = c_names)
colnames(expected_values) <- c("EX","VAR")
expected_values["(Intercept)",1] <- 110
expected_values["(Intercept)",2] <- 10
expected_values["race.completed",1] <- -.1
expected_values["race.completed",2] <- (0.2)^2
expected_values["tyre_medium",1] <- -(0.7*1000)/mean(race.laptimes$milliseconds)*100
expected_values["tyre_medium",2] <- (0.5)^2
expected_values["tyre_softer",1] <- -(1.4*1000)/mean(race.laptimes$milliseconds)*100
expected_values["tyre_softer",2] <- (0.8)^2
expected_values["tyre_age",1] <- (((hard_compound_deter_basis+medium_compound_deter+soft_compound_deter)/3)*1000)/mean(race.laptimes$milliseconds)*100
expected_values["tyre_age",2] <- (0.01)^2
expected_values["tyre_age.tyre_medium",1] <- ((medium_compound_deter - hard_compound_deter_basis)*1000)/mean(race.laptimes$milliseconds)*100
expected_values["tyre_age.tyre_medium",2] <- (0.01)^2
expected_values["tyre_age.tyre_softer",1] <- ((soft_compound_deter - hard_compound_deter_basis)*1000)/mean(race.laptimes$milliseconds)*100
expected_values["tyre_age.tyre_softer",2] <- (0.01)^2
expected_values["Main_driver",1] <- -0.5
expected_values["Main_driver",2] <- 1


priors <- data.frame(matrix(data=99,nrow=8,ncol=4),row.names = c_names); colnames(priors) <- c("normal_mu","normal_std","gamma_alpha","gamma_beta")

MM_gamma <- function(EX,VAR) {
  # alpha - shape parameter # beta - rate parameter
  # VAR = alpha/beta^2 # EX = alpha/beta # alpha = VAR*beta^2 =
  # EX = alpha/beta = VAR*beta --> beta = EX/VAR --> alpha = VAR*beta^2
  beta = EX/VAR
  alpha = VAR * beta^2
  ret <- c(alpha,beta)
  return(ret)
}

priors["(Intercept)",c("normal_mu","normal_std")] <- c(expected_values["(Intercept)","EX"],sqrt(expected_values["(Intercept)","VAR"]))
priors["tyre_age",c("gamma_alpha","gamma_beta")] <- MM_gamma(expected_values["tyre_age","EX"],expected_values["tyre_age","VAR"])
priors["race.completed",c("normal_mu","normal_std")] <- c(expected_values["race.completed","EX"],sqrt(expected_values["race.completed","VAR"]))
priors["tyre_medium",c("normal_mu","normal_std")] <- c(expected_values["tyre_medium","EX"],sqrt(expected_values["tyre_medium","VAR"]))
priors["tyre_softer",c("normal_mu","normal_std")] <- c(expected_values["tyre_softer","EX"],sqrt(expected_values["tyre_softer","VAR"]))
priors["tyre_age.tyre_medium",c("gamma_alpha","gamma_beta")] <- MM_gamma(expected_values["tyre_age.tyre_medium","EX"],expected_values["tyre_age.tyre_medium","VAR"])
priors["tyre_age.tyre_softer",c("gamma_alpha","gamma_beta")] <- MM_gamma(expected_values["tyre_age.tyre_softer","EX"],expected_values["tyre_age.tyre_softer","VAR"])
priors["Main_driver",c("normal_mu","normal_std")] <- c(expected_values["Main_driver","EX"],sqrt(expected_values["Main_driver","VAR"]))

# 0 - normal, 1- gamma
priors[,"distribution"] <- c(0,1,0,0,0,1,1,0)

summary(OLS_results)

y <- as.vector(race.laptimes$lap_time.relative)
N <- length(y)
X <- cbind(as.matrix(rep(1, N)),as.matrix(race.laptimes[,aux.names[!aux.names %in% '(Intercept)']]))
X <- t(X)
c_names <- names(summary(OLS_results)$coefficients[,2])
rownames(X) <- c_names
k <- nrow(priors)
k_backup <- k
l <- ncol(priors)

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

t_tail_fit <- stan(file = "t_regression(2).stan",
                   data = c("N", "k", "l","y", "X", "priors"),
                   iter = 5000,
                   chains = 8)

#saveRDS(t_tail_fit,"t_tail_fit5000.rds")
#t_tail_fit <- readRDS("t_tail_fit5000.rds")

#Przebieg ³añcuchów
rstan::traceplot(t_tail_fit, pars = c("h", "beta"), inc_warmup = FALSE, nrow = 6)
simulated.chains.1by1 <- extract(t_tail_fit,
                                 permuted = FALSE,
                                 inc_warmup = FALSE,
                                 par = c("beta"))

chain_len <- length(mcmc(simulated.chains.1by1[, 1, ])[,1])
discard <- 0
combined.chains <- mcmc.list(mcmc(simulated.chains.1by1[, 1, ][(round(discard*chain_len)+1):chain_len,]), 
                             mcmc(simulated.chains.1by1[, 2, ][(round(discard*chain_len)+1):chain_len,]),
                             mcmc(simulated.chains.1by1[, 3, ][(round(discard*chain_len)+1):chain_len,]),
                             mcmc(simulated.chains.1by1[, 4, ][(round(discard*chain_len)+1):chain_len,]),
                             mcmc(simulated.chains.1by1[, 5, ][(round(discard*chain_len)+1):chain_len,]),
                             mcmc(simulated.chains.1by1[, 6, ][(round(discard*chain_len)+1):chain_len,]),
                             mcmc(simulated.chains.1by1[, 7, ][(round(discard*chain_len)+1):chain_len,]),
                             mcmc(simulated.chains.1by1[, 8, ][(round(discard*chain_len)+1):chain_len,]))
plot(combined.chains) # dwumodalna gêstoœæ - ³añcuchy nie zbieg³y; (czy losuj¹ z tego samego posteriori)
# Trace - czy oscyluj¹ wokó³ tej samej œredniej, taka sama wariancja, czy nie rozje¿d¿aj¹ siê do +- Inf
summary(combined.chains, quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975)) # SD skorygowane o b³¹d numeryczny

#Numerical standard error (Koop, p. 65)
S0 <- spectrum0(combined.chains)
S_1 <- dim(simulated.chains.1by1)[1]
numerical.SE <- (S0$spec / S_1) ^ 0.5
#Raftery and Lewis diagnostics
raftery.diag(combined.chains, q = 0.5, r = 0.001, s = 0.95) # ile obserwacji potrzebujemy do oszacowania mediany (przy dok³adnoœci r)

#Scale reduction factors (Koop, p. 66)
gelman.diag(combined.chains) # < wartoœæ graniczna 1.2, a zatem wariancja wewn¹trzgrupowa dominuje wariancjê miêdzygrupow¹ - w takiej sytuacji wariancja wewn¹trz jednego ³añcucha jest mo¿liwie bliska wariancji po³¹czonych ³añcuchów
gelman.plot(combined.chains)

#Geweke statistics (Koop, p. 68)
geweke.diag(combined.chains, frac1 = 0.1, frac2 = 0.5)
geweke.plot(combined.chains, frac1 = 0.1, frac2 = 0.5, nbins = 40, pvalue = 0.05)

#Heidel
heidel.diag(combined.chains) # 

#Serial correlation in chains
autocorr(combined.chains, lags = c(0, 1, 5, 10, 50), relative = TRUE)
autocorr.diag(combined.chains)
autocorr.plot(combined.chains, lag.max = 50)
#Cross correlation in chains
crosscorr(combined.chains)
par(mfrow = c(1, 1))
crosscorr.plot(combined.chains)


simulated.chains.all <- extract(t_tail_fit,
                                inc_warmup = FALSE,
                                par = c("h", "beta"))

#print results
mean_vals <- data.frame(matrix(data=NA,nrow=k_backup,ncol=3))
rownames(mean_vals) <- rownames(priors)
colnames(mean_vals) <- c("OLS coef","Posteriori mean","W95n / W95t")

for (ii in 1:k_backup){
  print(paste(rownames(priors)[ii],sep=""))
  monitor(simulated.chains.all$beta[,ii])
  mean_vals[ii,"OLS coef"] <- summary(OLS_results)$coefficients[ii,1]
  mean_vals[ii,"Posteriori mean"] <- monitor(simulated.chains.all$beta[,ii])$mean
  #mean_vals[ii,"Posteriori mean"] <- monitor(simulated.chains.all$beta[,ii])$`2.5%`
  #mean_vals[ii,"Posteriori mean"] <- monitor(simulated.chains.all$beta[,ii])$mean
}
print(mean_vals)

library(coda)
#Szerokoœæ HPDI vs przedzia³y ufnoœci w analizie klasycznej
chain.b <- mcmc(simulated.chains.all$beta)
hpdi.b <- HPDinterval(chain.b)
c_names <- names(summary(OLS_results)$coefficients[,2])
rownames(hpdi.b) <- c_names
width95n <- confint(OLS_results)[, 2] - confint(OLS_results)[, 1]
width95t <- hpdi.b[, 2] - hpdi.b[, 1]
mean_vals[,"W95n / W95t"] <- as.matrix(width95n / width95t,nrow=k_backup,ncol=1)
#print(round(mean_vals,3),type="html")

#Wizualizacja gêstoœci brzegowych a posteriori
grey_area <- rgb(160, 160, 160, 80, names = NULL, maxColorValue = 255)
grey_line <- rgb(80, 80, 80, 160, names = NULL, maxColorValue = 255)
green_area <- rgb(24, 121, 104, 80, names = NULL, maxColorValue = 255)
green_line <- rgb(13, 85, 72, 160, names = NULL, maxColorValue = 255)

n_eval_points = 10001
beta.space.Intercept <- seq(from=109,to=111,length.out=n_eval_points)#check
beta.space.tyre_age <- seq(from = 0.05, to = 0.18, length.out=n_eval_points)
beta.space.tyre_age.sq <- seq(from = -0.0001*100, to = 0.00002*100, length.out=n_eval_points)
beta.space.race.completed <- seq(from = -.06, to = -0.02, length.out=n_eval_points)
beta.space.tyre_medium <- seq(from = -1.5, to = .5, length.out=n_eval_points)
beta.space.tyre_age.tyre_medium <- seq(from = 0, to = .1, length.out=n_eval_points)
beta.space.tyre_softer <- seq(from = -2, to = 0, length.out=n_eval_points)
beta.space.tyre_age.tyre_softer <- seq(from = 0.04, to = .11, length.out=n_eval_points)
beta.space.Main_driver <- seq(from = -1, to = 0.5, length.out=n_eval_points)

k <- k_backup
prior.marg.dens.beta <- matrix(NA, nrow = k, ncol = n_eval_points)

for(ii in 1:k) {
  beta.space.choice <- rownames(priors)[ii];if(beta.space.choice=="(Intercept)"){beta.space.choice="Intercept"}
  eval(parse(text = paste("beta.space <- beta.space.",beta.space.choice, sep="")))
  distribution <- priors[ii,"distribution"]
  if (distribution == 0) {
    mu = priors[ii,"normal_mu"]
    std = priors[ii,"normal_std"]
    prior.marg.dens.beta[ii, ] <- apply(as.matrix(beta.space), 1, dnorm, 
                                        mean = mu, sd = std, log = FALSE)
  } else if (distribution == 1) {
    alpha = priors[ii,"gamma_alpha"]
    beta = priors[ii,"gamma_beta"]
    prior.marg.dens.beta[ii, ] <- apply(as.matrix(beta.space), 1, dgamma, 
                                        shape = alpha, rate = beta, log = FALSE)
  }
}

#plot(beta.space,prior.marg.dens.beta[ii,])

par(mfrow = c(1,1))
for (jj in c(1,5)) {
  windows(width = 1200, height = 800)
  par(mfrow = c(2,2))
  for (ii in  jj:(jj+3)) {
    
    beta.space.choice <- rownames(priors)[ii];if(beta.space.choice=="(Intercept)"){beta.space.choice="Intercept"}
    eval(parse(text = paste("beta.space <- beta.space.",beta.space.choice, sep="")))
    distribution <- priors[ii,"distribution"]

    if (distribution == 0) {
      distribution_name = "N"
      param1 = round(priors[ii,"normal_mu"],2)
      param2 = round(priors[ii,"normal_std"],2)
    } else if (distribution == 1) {
      distribution_name = "Gamma"
      param1 = round(priors[ii,"gamma_alpha"],2)
      param2 = round(priors[ii,"gamma_beta"],2)
    }
    
    par(mar = c(5, 4, 4, 4) + 0.7)
    hist(simulated.chains.all$beta[,ii],main=paste("Posterior density of ",c_names[ii]),
         xlab="Parameter value",ylab="Frequency",col = green_area, xlim=c(min(beta.space),max(beta.space)))
    abline(v=monitor(simulated.chains.all$beta[,ii])$mean, col = green_line, lwd = 3)
    abline(v=OLS_results$coefficients[ii],col="black",lwd=3)
    par(new = TRUE)
    plot(beta.space,prior.marg.dens.beta[ii,],type = "l", axes = FALSE, xlab = "", ylab = "",
         las = 1, lwd = 2, bty = "n", col = grey_area,)
    axis(side=4, at = pretty(range(prior.marg.dens.beta[ii,])))
    polygon(c(beta.space[1], beta.space, beta.space[n_eval_points]), c(min(prior.marg.dens.beta[ii, ]), prior.marg.dens.beta[ii, ], min(prior.marg.dens.beta[ii, ])),    
            col=grey_area, border=NA)
    #if (beta.space.choice %in% c("Intercept","Main_driver")) {
    #  polygon(beta.space, prior.marg.dens.beta[ii, ], col = grey_area, border = NA)
    #  } else {
    #    polygon(c(beta.space, rev(beta.space)), c(prior.marg.dens.beta[ii, ],
    #                                          rep(0, length(beta.space))), col = grey_area, border = NA)
    #  }
    EX <- ifelse(distribution == 0, priors[ii,"normal_mu"],priors[ii,"gamma_alpha"]/priors[ii,"gamma_beta"])
    abline(v=EX,col=grey_line,lwd=3)
    mtext("z", side=4, line=3)
    legend(x = "topright",legend = c(paste("Prior density ~\n",distribution_name,"(",param1,"; ",param2,")",sep=""), "Posterior density"), 
           fill = c(grey_area, green_area))
  }
  par(mfrow = c(1,1))
}

# Visualise CI?
par(mfrow = c(2, 1))
priors
conf_level = 95
red_area <- rgb(255, 100, 123, 60, names = NULL, maxColorValue = 255)
red_line <- rgb(200, 0, 30, 160, names = NULL, maxColorValue = 255)

par(mfrow = c(1,1))
for (jj in c(1,5)) {
  windows(width = 1200, height = 1000)
  par(mfrow = c(2,2))
  for (ii in  jj:(jj+3)) {
    
    #beta.space.choice <- rownames(priors)[ii];if(beta.space.choice=="(Intercept)"){beta.space.choice="Intercept"}
    #eval(parse(text = paste("beta.space <- beta.space.",beta.space.choice, sep="")))
    distribution <- priors[ii,"distribution"]
    
    if (distribution == 0) {
      distribution_name = "N"
      param1 = round(priors[ii,"normal_mu"],2)
      param2 = round(priors[ii,"normal_std"],2)
    } else if (distribution == 1) {
      distribution_name = "Gamma"
      param1 = round(priors[ii,"gamma_alpha"],2)
      param2 = round(priors[ii,"gamma_beta"],2)
    }
    
    par(mar = c(5, 4, 4, 4) + 0.7)
    hist(simulated.chains.all$beta[,ii],main=paste("Posterior density of ",c_names[ii]),
         xlab="Parameter value",ylab="Frequency",col = green_area)
    abline(v=monitor(simulated.chains.all$beta[,ii])$mean, col = green_line, lwd = 3)
    width95t <- hpdi.b[, 2] - hpdi.b[, 1]
    abline(v=hpdi.b[ii,1],col=red_line,lwd=2)
    abline(v=hpdi.b[ii,2],col=red_line,lwd=2)
    
    beta.space <- simulated.chains.all$beta[,ii][simulated.chains.all$beta[,ii]>= hpdi.b[ii,1] & simulated.chains.all$beta[,ii]<= hpdi.b[ii,1]]
    
    rect(xleft = hpdi.b[ii,1], xright = hpdi.b[ii,2], ybottom = 0, ytop = par("usr")[4], 
         border = NA, col = red_area)
    legend(x = "topright",legend = c("Posterior density", "95% CI"), 
           fill = c(green_area,red_area))
  }
  par(mfrow = c(1,1))
}

# Czynniki Bayesa
if(!require("bridgesampling")) {install.packages("bridgesampling"); library(bridgesampling)}
marg_lik <- bridge_sampler(t_tail_fit)
print(marg_lik)

# do przerobienia!!!
#Zachowaj dane dla pe³nej macierzy X
X_backup <- X
priors_backup <- priors

#Kontenery na wyniki
race_pace_fit <- list()
marg_lik2 <- list()

#Modele i i ich gêstoœci brzegowe bez jednej zmiennej objaœniaj¹cej
for (ii in 1:7) {
  
  X <- X_backup[-(ii+1),]
  priors <- priors_backup[-(ii+1),]
  #beta_prior <- beta_prior_backup[-(ii+1)]
  #U_prior <- U_prior_backup[-(ii+1), -(ii+1)]
  k <- nrow(X)
  
  race_pace_fit[[ii]] <- stan(file = "t_regression(2).stan",
                     data = c("N", "k", "l","y", "X", "priors"),
                     iter = 5000, warmup=2500,
                     chains = 8)
  
  marg_lik2[[ii]] <- bridge_sampler(race_pace_fit[[ii]])
}

print(marg_lik2)

#Przywrócenie danych dla pe³nej macierzy X
X <- X_backup
priors <- priors_backup
#beta_prior <- beta_prior_backup
#U_prior <- U_prior_backup
BF_num_1 <- rep(NA, 7)
BF_num_2 <- rep(NA, 7)
BF_num_3 <- rep(NA, 7)

for (ii in 1:7) {
  BF_num_1[ii] <- exp(marg_lik$logml) / exp(marg_lik2[[ii]]$logml)
  BF_num_2[ii] <- bayes_factor(marg_lik, marg_lik2[[ii]])$bf
  BF_num_3[ii] <- bf(marg_lik, marg_lik2[[ii]])$bf
}

BF_1_2_table <- cbind(BF_num_1, BF_num_2, BF_num_3)
#BF_1_2_table <- cbind(BF_1_2_table, BF_num_1, BF_num_2, BF_num_3)
rownames(BF_1_2_table) <- rownames(X_backup)[-1]

BF_1_2_table <- data.frame(round(BF_1_2_table,digits=3)[,1])
colnames(BF_1_2_table) <- "BF"
BF_1_2_table['1-3 |'] <- ifelse(BF_1_2_table$BF>=1 & BF_1_2_table$BF<=3,"Here","")
BF_1_2_table[' 3-20 |'] <- ifelse(BF_1_2_table$BF>3 & BF_1_2_table$BF<=20,"Here","")
BF_1_2_table[' 20-150 |'] <- ifelse(BF_1_2_table$BF>20 & BF_1_2_table$BF<=150,"Here","")
BF_1_2_table[' >150'] <- ifelse(BF_1_2_table$BF>150,"Here","")