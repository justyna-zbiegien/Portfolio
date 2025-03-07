# Czyszczenie przestrzeni roboczej, wykres?w i konsoli ---------------------------
rm(list = ls())
if(!is.null(dev.list())) dev.off()
cat("\014")


# Pakiety, dane, kolory do wykres?w ------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)
library(corrplot)
library(foreign)
if(!require("plm")) {install.packages("plm"); library(plm)}
if(!require("manipulate")) {install.packages("manipulate"); library(manipulate)}
if(!require("metRology")) {install.packages("metRology"); library(metRology)}
if(!require("mvtnorm")) {install.packages("mvtnorm"); library(mvtnorm)}
library(fitdistrplus)
library(manipulate)
library(logspline)
library(metRology) # skorzystamy z funkcji dt.scaled()
grey_area <- rgb(160, 160, 160, 80, names = NULL, maxColorValue = 255)
grey_line <- rgb(80, 80, 80, 160, names = NULL, maxColorValue = 255)
green_area <- rgb(24, 121, 104, 80, names = NULL, maxColorValue = 255)
green_line <- rgb(13, 85, 72, 160, names = NULL, maxColorValue = 255)


# wgrywanie i przygotowywanie danych -----------------------------------------------
directory.main <- dirname(rstudioapi::getSourceEditorContext()$path)

dane <- read.csv("dane_happy.csv", sep = ",", dec=".", stringsAsFactors=FALSE)

nulls <- sapply(dane, function(x) sum(is.na(x)))
nulls
dane <- na.omit(dane)

dane = dane[2016< dane$year,]


# przygotowanie danych dla modelu pomocniczego glm------------------------------------------------------

for(i in 1:length(dane$lifeladder))
{
  if(dane$lifeladder[i]<=5)
    { dane$lifeladder[i]=0}
  else 
    {dane$lifeladder[i]=1}
}

lifeladder <-dane$lifeladder
gdp <- dane$loggdppercapita
socialsup <- dane$socialsupport
health<-dane$healthylifeexpectancyatbirth
freedom <- dane$freedomtomakelifechoices
generos <- dane$generosity
corrupt <- dane$perceptionsofcorruption
positive <- dane$positiveaffect
negative <- dane$negativeaffect
democr <- dane$democraticquality
delivery <- dane$deliveryquality

names(dane)[4]<-"gdp"
names(dane)[5]<-"socialsup"
names(dane)[6]<-"health"
names(dane)[7]<-"freedom"
names(dane)[8]<-"generos"
names(dane)[9]<-"corrupt"
names(dane)[10]<-"positive "
names(dane)[11]<-"negative"
names(dane)[12]<-"democr"
names(dane)[13]<-"delivery"


# Oszadowanie modelu logiowego (glm)---------------------------------

model <- glm(lifeladder ~ gdp + socialsup +health+ freedom + generos + corrupt+ positive+negative+ democr+delivery,  family = "binomial",data=dane)
summary(model)

summary(dane)
kor<-cor(dane[3:13])
corrplot(kor, method = "number")

model <-glm(lifeladder ~ gdp + socialsup +health+negative+delivery, family = "binomial", data=dane)
summary(model)

N <- nrow(dane)
k <- 6

#Rstan----------------------------------------------
library("rstan")

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

start <- Sys.time()
model_stan <- stan(file = "jz_projekt_2.stan",
                   data = c("N", "lifeladder","gdp", "socialsup" ,"health", "negative", "delivery", "k"),
                   iter= 1000,
                   chains = 4)
end <- Sys.time()
end - start

print(model_stan)

simulated.chains.all <- extract(model_stan,
                                inc_warmup = FALSE,
                                par = "beta")

# g?sto?ci brzegowe a posteriori--------------------------------------
par(mfrow = c(3,2))
for (ii in  1:5) {
  hist(simulated.chains.all$beta[,ii+1], xlab = "", main = c("gdp", "socialsup" ,"health", "negative", "delivery")[ii], col=green_area)
}


#Szeroko?? HPDI vs przedzia?y ufno?ci w analizie klasycznej
library(coda)
chain.b <- mcmc(simulated.chains.all$beta)
hpdi.b <- HPDinterval(chain.b)
rownames(hpdi.b) <- c("(Intercept)", "gdp", "socialsup" ,"health", "negative", "delivery")
(hpdi.b)
width95_model <- confint(model)[, 2] - confint(model)[, 1]
width95_model
width95_model_stan <- hpdi.b[, 2] - hpdi.b[, 1]
width95_model_stan
width95_model / width95_model_stan



#Czynniki Bayesa
library(bayestestR)

model2 <- glm(lifeladder ~  socialsup +health+negative+delivery, family = "binomial", data=dane)
model3 <- glm(lifeladder ~ gdp  +health+negative+delivery, family = "binomial", data=dane)
model4 <- glm(lifeladder ~ gdp + socialsup +negative+delivery, family = "binomial", data=dane)
model5 <- glm(lifeladder ~ gdp + socialsup +health+delivery, family = "binomial", data=dane)
model6 <- glm(lifeladder ~ gdp + socialsup +health+negative, family = "binomial", data=dane)

(BFM <- bayesfactor_models(model, model2, model3, model4, model5, model6, denominator = 1))

Bayes_factors <- 1 / exp(BFM$log_BF)
BF <- data.frame(c("(Intercept)", "gdp", "socialsup" ,"health", "negative", "delivery"), Bayes_factors[1:6])
colnames(BF) <- c("Zmienna", "BF")
BF
