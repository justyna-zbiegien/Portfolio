#Czyszczenie przestrzeni roboczej, wykresów i konsoli
rm(list = ls())
if(!is.null(dev.list())) dev.off()
cat("\014")

#Pakiety, dane, kolory do wykresów
library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)
library(corrplot)
if(!require("manipulate")) {install.packages("manipulate"); library(manipulate)}
if(!require("metRology")) {install.packages("metRology"); library(metRology)}
if(!require("mvtnorm")) {install.packages("mvtnorm"); library(mvtnorm)}
library(manipulate)
library(metRology) # skorzystamy z funkcji dt.scaled()
grey_area <- rgb(160, 160, 160, 80, names = NULL, maxColorValue = 255)
grey_line <- rgb(80, 80, 80, 160, names = NULL, maxColorValue = 255)
green_area <- rgb(24, 121, 104, 80, names = NULL, maxColorValue = 255)
green_line <- rgb(13, 85, 72, 160, names = NULL, maxColorValue = 255)


#wgrywanie i przygotowywanie danych

dane <- read.csv("C:/Justyna/Zadania/Mgr/sem IV/Bayesowska/projekt1/dane/bgg_dataset3.csv", sep = ";", dec=".", stringsAsFactors=FALSE)
set.seed(1111)
dane %>% glimpse()
dane<-dane[,-1]

dane<-subset(dane, select= -c(Users.Rated, Owned.Users, Mechanics, Name))

nulls <- sapply(dane, function(x) sum(is.na(x)))
nulls
dane <- na.omit(dane)

cor(dane$Mechanics_count, dane$Rating.Average)

#rozbicie zmiennej Domains na zmienne binarne

Domains<-unique(trimws(unlist(strsplit(as.character(dane$Domains), ","))))


for (i in 15: (length(Domains) + ncol(dane))){
  dane[, i] <- NA
}
col.from<-colnames(dane)[15:ncol(dane)]
dane<-dane %>% rename_at(vars(col.from), function(x) Domains)


for( i in 1:nrow(dane))
{
  for( j in 15:ncol(dane))
    if(grepl(colnames(dane)[j],dane$Domains[i], fixed = TRUE))
       {
         dane[i,j]<-1
    }else
    {
      dane[i,j]<-0
    }
}

#zmiana nazw Domains i usuwanie kolumny
Domains <- sub(" ", "_", Domains)
Domains <- sub("'", "", Domains)
col.from<-colnames(dane)[15:ncol(dane)]
dane<-dane %>% rename_at(vars(col.from), function(x) Domains)

dane<-subset(dane, select= -Domains)

dane1 = dane[2019< dane$Year.Published,]
dane1 = dane1[ dane1$Year.Published<2021,]
dane2 = dane[dane$Year.Published > 2020,]
dane1<-subset(dane1, select= -c(Year.Published, Strategy_Games, Thematic_Games, Abstract_Games, Family_Games, Party_Games, Childrens_Games, Mechanics_count, Customizable_Games))
dane2<-subset(dane2, select= -c(Year.Published, Strategy_Games, Thematic_Games, Abstract_Games, Family_Games, Party_Games, Childrens_Games, Mechanics_count, Customizable_Games))


#sprawdzanie korelacji
M<-cor(dane)
corrplot(M, method="circle")

#OSZACOWANIE KLASYCZNE (KMNK)

kmnk1<-lm(Rating.Average ~  Min.Players + BGG.Rank + Complexity.Average  + Wargames + Variable.player.powers + Dice.rolling, data=dane1)
summary(kmnk1)

kmnk2<-lm(Rating.Average ~  Min.Players + BGG.Rank + Complexity.Average  + Wargames + Variable.player.powers + Dice.rolling, data=dane2)
summary(kmnk2)

#sprawdzanie rozk³¹du normalnego reszt
shapiro.test(kmnk2$residuals)
hist(kmnk2$residuals, col = green_line, main = "Histogram reszt", xlab = "reszty")

dane1<-subset(dane1, select= c(Rating.Average, Min.Players, BGG.Rank, Complexity.Average, Wargames, Variable.player.powers, Dice.rolling ))
dane2<-subset(dane2, select= c(Rating.Average, Min.Players, BGG.Rank, Complexity.Average, Wargames, Variable.player.powers, Dice.rolling ))

# parametry a priori

y <- as.matrix(dane2[, c('Rating.Average')])
N.data <- length(y)
X <- cbind(as.matrix(rep(1, N.data)), 
           as.matrix(dane2[, c("Min.Players", "BGG.Rank", "Complexity.Average", "Wargames", "Variable.player.powers", "Dice.rolling")]))
Beta.ols.data <- kmnk2$coefficients
v.data <- kmnk2$df.residual
XTX.data <- t(X) %*% X
s2.data <- sum((kmnk2$residuals) ^ 2) / v.data


Beta.prior <- c(7.547, 0.0059, -0.00006, 0.1678, 0.4563, 0.1093, -0.0511)
sm2.prior <- 4 # s2.prior = 0.25, s.prior = 0.5
U.prior <- 0.1 * 7 * diag(7) # zerowe kowariancje i wariancja (co do wartoœci oczekiwanej) 0.1 * 4 * (1/4) = 0.1 (tzn. odchylenie standardowe (0.1)^0.5 = 0.31)
U.prior[1, 1] <- 200
v.prior <- 145
k <- ncol(X)
N <- length(y)

#4. Parametry a posteriori

Beta.posterior <- solve(solve(U.prior) + XTX.data) %*% (solve(U.prior) %*% Beta.prior + XTX.data %*% Beta.ols.data)
U.posterior <- solve(solve(U.prior) + XTX.data)
v.posterior <- v.prior + N.data
vs2.posterior <- v.prior / sm2.prior + v.data * s2.data + t(Beta.ols.data-Beta.prior) %*% solve(U.prior + solve(XTX.data)) %*% (Beta.ols.data - Beta.prior)
sm2.posterior <- 1 / (vs2.posterior / v.posterior)

#5. Dane do wykresów - gêstoœæ a priori i a posteriori
beta.space <- seq(from = -2, to = 2, by = 0.01)
n_eval_points <- length(beta.space)
n_parameters <- length(Beta.posterior)
prior.marg.dens.beta <- matrix(NA, nrow = n_parameters, ncol = n_eval_points)
posterior.marg.dens.beta <- matrix(NA, nrow = n_parameters, ncol = n_eval_points)
for(ii in 1:length(Beta.posterior)) {
  prior.marg.dens.beta[ii, ] <- apply(as.matrix(beta.space), 1, dmvt,
                                      delta = Beta.prior[ii], sigma = as.matrix(U.prior[ii, ii] / sm2.prior), df = v.prior, log = FALSE)
  posterior.marg.dens.beta[ii, ] <- apply(as.matrix(beta.space), 1, dmvt,
                                          delta = Beta.posterior[ii],sigma = as.matrix(U.posterior[ii, ii] / sm2.posterior), df = v.posterior, log = FALSE)
}


for(ii in 2:length(Beta.posterior)) {
  plot(beta.space, prior.marg.dens.beta[ii, ], las = 1, lwd = 2, bty = "n", col = grey_area,
       ylim = c(0, max(c(max(prior.marg.dens.beta[ii, ]),max(posterior.marg.dens.beta[ii, ]))) + 1), type = "l", ylab = "gêstoœæ", main = colnames(dane2)[ii])
  polygon(c(beta.space, rev(beta.space)), c(prior.marg.dens.beta[ii, ], 
                                            rep(0, length(beta.space))), col = grey_area, border = NA)
  abline(v = Beta.prior[ii], col = grey_line, lwd = 3)
  text(Beta.prior[ii], max(prior.marg.dens.beta[ii, ]) + 0.4, paste("E(beta) a priori = ", Beta.prior[ii]), col = grey_line)
  abline(v = Beta.ols.data[ii], col = rgb(0, 0, 0, 1), lwd = 3)
  text(Beta.ols.data[ii], max(posterior.marg.dens.beta[ii, ]) + 0.2, paste("parametr OLS = ", round(Beta.ols.data[ii], 4)), col = rgb(0, 0, 0, 1))
  lines(beta.space, posterior.marg.dens.beta[ii, ], lwd = 2, col = green_line)
  polygon(c(beta.space, rev(beta.space)), c(posterior.marg.dens.beta[ii, ], 
                                            rep(0, length(beta.space))), col = green_area, border = NA)
  abline(v = Beta.posterior[ii], col = green_line, lwd = 3)
  text(Beta.posterior[ii], max(posterior.marg.dens.beta[ii, ]) + 0.6, paste("E(beta) a posteriori = ", round(Beta.posterior[ii], digits = 4)), col = green_line)
}

# HPDI
red_area <- rgb(255, 100, 123, 80, names = NULL, maxColorValue = 255)
red_line <- rgb(200, 0, 30, 160, names = NULL, maxColorValue = 255)


for( ii in 1:ncol(dane2)){
par(mfrow = c(1, 1))
manipulate( 
  {#Tworzymy zmienn¹ binarn¹ wskazuj¹c¹, gdzie bêdzie HPDI - tzn. o najwy¿ej gêstoœci a posteriori ponad zadany poziom
    highest <- data.frame(1:n_eval_points, posterior.marg.dens.beta[ii, ], stringsAsFactors = FALSE)
    highest <- highest[order(highest[, 2], decreasing = TRUE), ]
    highest <- data.frame(highest, cumsum(highest[, 2]) < conf_level, stringsAsFactors = FALSE)
    highest <- highest[order(highest[, 1], decreasing = FALSE), ]
    credible_set_indicator <- as.vector(as.integer(highest[, 3]))
    credible_set_begin <- match(1, credible_set_indicator)
    credible_set_end <- length(credible_set_indicator) - match(1, rev(credible_set_indicator))
    #Lewy i prawy brzeg HPDI
    x1 <- beta.space[credible_set_begin]
    x2 <- beta.space[credible_set_end]
    #Na potrzeby wykresu tworzymy wektor, który przyjmuje wartoœæ gêstoœci a posteriori w HPDI i zero poza nim
    posterior.cs <- posterior.marg.dens.beta[ii, ] * credible_set_indicator
    #Poziom ufnoœci
    HPDI_probab <- conf_level * 0.01
    #Wykres gêstoœci a posteriori
    plot(beta.space, posterior.marg.dens.beta[ii, ], las = 1, lwd = 2, bty = "n", col = green_line,
         ylim = c(0, max(posterior.marg.dens.beta[ii, ] + 1)), type = "l", ylab = "gêstoœæ", main = colnames(dane2)[ii])
    polygon(c(beta.space, rev(beta.space)), 
            c(posterior.marg.dens.beta[ii, ], rep(0, length(beta.space))), 
            col = green_area, border = NA)
    text(Beta.posterior[ii], max(posterior.marg.dens.beta[ii, ]) + 0.6, paste("E(beta) a posteriori = ", round(Beta.posterior[ii], digits = 4)), col = green_line)
    abline(v = Beta.posterior[ii], col = green_line, lwd = 3)
    #Pole oznaczaj¹ce gêstoœæ a posteriori w przedziale ufnoœci HPDI
    polygon(c(beta.space, rev(beta.space)), 
            c(posterior.cs, rep(0, length(beta.space))), 
            col = red_area, border = NA)
    
    #Wyœwietl poziom ufnoœci i granice przedzia³u
    text(0, max(posterior.marg.dens.beta[ii, ]) + 0.2, paste(round(HPDI_probab * 100, digits = 1), "% przedzia³ HPDI: (", round(x1, digits = 2), " , ", round(x2, digits = 2), ")"), col = red_line)
  },
  conf_level = slider(0, 100, step = 1, initial = 95)
)
}

#plot dla BGG Rank

beta.space <- seq(from = -0.02, to = 0.02, by = 0.001)
n_eval_points <- length(beta.space)
n_parameters <- length(Beta.posterior)
prior.marg.dens.beta <- matrix(NA, nrow = n_parameters, ncol = n_eval_points)
posterior.marg.dens.beta <- matrix(NA, nrow = n_parameters, ncol = n_eval_points)
for(ii in 1:length(Beta.posterior)) {
  prior.marg.dens.beta[ii, ] <- apply(as.matrix(beta.space), 1, dmvt,
                                      delta = Beta.prior[ii], sigma = as.matrix(U.prior[ii, ii] / sm2.prior), df = v.prior, log = FALSE)
  posterior.marg.dens.beta[ii, ] <- apply(as.matrix(beta.space), 1, dmvt,
                                          delta = Beta.posterior[ii],sigma = as.matrix(U.posterior[ii, ii] / sm2.posterior), df = v.posterior, log = FALSE)
}
ii=3

par(mfrow = c(1, 1))
manipulate( 
  {#Tworzymy zmienn¹ binarn¹ wskazuj¹c¹, gdzie bêdzie HPDI - tzn. o najwy¿ej gêstoœci a posteriori ponad zadany poziom
    highest <- data.frame(1:n_eval_points, posterior.marg.dens.beta[ii, ], stringsAsFactors = FALSE)
    highest <- highest[order(highest[, 2], decreasing = TRUE), ]
    highest <- data.frame(highest, cumsum(highest[, 2]) < conf_level, stringsAsFactors = FALSE)
    highest <- highest[order(highest[, 1], decreasing = FALSE), ]
    credible_set_indicator <- as.vector(as.integer(highest[, 3]))
    credible_set_begin <- match(1, credible_set_indicator)
    credible_set_end <- length(credible_set_indicator) - match(1, rev(credible_set_indicator))
    #Lewy i prawy brzeg HPDI
    x1 <- beta.space[credible_set_begin]
    x2 <- beta.space[credible_set_end]
    #Na potrzeby wykresu tworzymy wektor, który przyjmuje wartoœæ gêstoœci a posteriori w HPDI i zero poza nim
    posterior.cs <- posterior.marg.dens.beta[ii, ] * credible_set_indicator
    #Poziom ufnoœci
    HPDI_probab <- conf_level * 0.01
    #Wykres gêstoœci a posteriori
    plot(beta.space, prior.marg.dens.beta[ii, ], las = 1, lwd = 2, bty = "n", col = grey_area,
         ylim = c(0, 5), type = "l", ylab = "gêstoœæ", main = colnames(dane2)[ii])
    polygon(c(beta.space, rev(beta.space)), c(prior.marg.dens.beta[ii, ], 
                                              rep(0, length(beta.space))), col = grey_area, border = NA)
    abline(v = Beta.prior[ii], col = grey_line, lwd = 3)
    text(Beta.prior[ii], max(prior.marg.dens.beta[ii, ]) + 0.4, paste("E(beta) a priori = ", Beta.prior[ii]), col = grey_line)
    abline(v = Beta.ols.data[ii], col = rgb(0, 0, 0, 1), lwd = 3)
    text(Beta.ols.data[ii], 3 + 0.2, paste("parametr OLS = ", round(Beta.ols.data[ii], 8)), col = rgb(0, 0, 0, 1))
    lines(beta.space, posterior.marg.dens.beta[ii, ], lwd = 2, col = green_line)
    polygon(c(beta.space, rev(beta.space)), c(posterior.marg.dens.beta[ii, ], 
                                              rep(0, length(beta.space))), col = green_area, border = NA)
    abline(v = Beta.posterior[ii], col = green_line, lwd = 3)
    text(Beta.posterior[ii], 4 + 0.6, paste("E(beta) a posteriori = ", round(Beta.posterior[ii], digits = 8)), col = green_line)
    #Pole oznaczaj¹ce gêstoœæ a posteriori w przedziale ufnoœci HPDI
    polygon(c(beta.space, rev(beta.space)), 
            c(posterior.cs, rep(0, length(beta.space))), 
            col = red_area, border = NA)
    
    #Wyœwietl poziom ufnoœci i granice przedzia³u
    text(0, max(posterior.marg.dens.beta[ii, ]) + 0.2, paste(round(HPDI_probab * 100, digits = 1), "% przedzia³ HPDI: (", round(x1, digits = 2), " , ", round(x2, digits = 2), ")"), col = red_line)
  },
  conf_level = slider(0, 100, step = 1, initial = 95)
)



# Wyznaczamy wiarygodnoœci brzegowe 2 modeli: ze wszystkimi zmiennymi i bez zmiennej log_Income

## Model 1: ze wszystkimi 
#obliczenia mamy ju¿ gotowe
vs2.prior <- v.prior / sm2.prior
### TUTAJ SPRAWDZIÆ DLACZEGO WYCHODZI NAN
P_y_M1 <- ((det(U.posterior) ^ 0.5) * gamma(v.posterior / 2) * ((vs2.posterior) ^ (- v.posterior / 2))) / ((pi ^ (N.data / 2)) * (det(U.prior) ^ 0.5) * gamma(v.prior / 2) * ((vs2.prior) ^ (- v.prior / 2)))

## Model 2: bez jednej zmiennej 
P_y_M2 <- rep(NA, 6)
for (ii in 2:7) {
  #Powtarzamy obliczenia z ograniczon¹ macierz¹ X...
  X_2 <- X[, -c(ii)]
  eval(parse(text = paste("OLS_results_2 <- lm(Rating.Average ~ ", paste(colnames(X_2), collapse = "+"), ", data = dane2)", sep = "")))
  Beta.ols.data_2 <- OLS_results_2$coefficients
  v.data_2 <- OLS_results_2$df.residual
  XTX.data_2 <- t(X_2) %*% X_2
  s2.data_2 <- sum((OLS_results_2$residuals) ^ 2) / v.data_2
  
  #Uwaga na inny rozk³ad a priori (ni¿sza liczba wymiarów wektora beta)
  Beta.prior_2 <- Beta.prior[-c(ii)]
  U.prior_2 <- U.prior[- c(ii), -c(ii)]
  #Zak³adamy, ¿e rozk³ad brzegowy precyzji pozostanie bez zmian
  sm2.prior_2 <- sm2.prior
  v.prior_2 <- v.prior
  vs2.prior_2 <- vs2.prior
  
  Beta.posterior_2 <- solve(solve(U.prior_2) + XTX.data_2) %*% (solve(U.prior_2) %*% Beta.prior_2 + XTX.data_2 %*% Beta.ols.data_2)
  U.posterior_2 <- solve(solve(U.prior_2) + XTX.data_2)
  v.posterior_2 <- v.prior_2 + N.data
  vs2.posterior_2 <- v.prior_2 / sm2.prior_2 + v.data_2 * s2.data_2 + t(Beta.ols.data_2 - Beta.prior_2) %*% solve(U.prior_2 + solve(XTX.data_2)) %*% (Beta.ols.data_2 - Beta.prior_2)
  sm2.posterior_2 <- 1 / (vs2.posterior_2 / v.posterior_2)
  
  #Tak jak w modelu 1 (pe³nym) obliczamy gêstoœæ brzegow¹:
  P_y_M2[ii - 1] <- ((det(U.posterior_2) ^ 0.5) * gamma(v.posterior_2 / 2) * ((vs2.posterior_2) ^ (- v.posterior_2 / 2))) / ((pi ^ (N.data / 2)) * (det(U.prior_2) ^ 0.5) * gamma(v.prior_2 / 2) * ((vs2.prior_2) ^ (- v.prior_2 / 2)))
}

# 2. Wyznaczamy czynniki Bayesa "dla poszczególnych zmiennych"
# (czyli, precyzyjniej, dla modeli z tymi zmiennymi i bez nich)
BF_1_2 <- P_y_M1 / P_y_M2
BF_1_2_table <- data.frame(names(Beta.ols.data[2:7]), BF_1_2)
colnames(BF_1_2_table) <- c("zmienna", "czynnik Bayesa (analitycznie)")
(BF_1_2_table)

