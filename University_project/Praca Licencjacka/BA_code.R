library(dplyr)
library(nlme)
library(car)
library(randtests)
library(lmtest)
library(strucchange)
library(tseries)
library(stats)

dane <- read.table(file = "C:\\Users\\Justyna\\Desktop\\Zadania\\IiE 3.1\\pracka\\dane\\dane_scalone_3.csv", 
                   sep=";", dec=";", header=TRUE)

dane<-dane[,c(-9, -14)]
head(dane)
colnames(dane)[1]<-"y"
Dane<-dane[,1:19]
head(Dane)
str(Dane)


summary(Dane)

Dane<-na.omit(Dane)  #usuwam braki danych 
summary(Dane)

Dane<-Dane[,-16]
Dane<-Dane[,-16]

#usuwanie danych odstaj?cych

k<-quantile(Dane$y)
k1<-k[2]
k3<-k[4]
r<-3*(k3-k1)/2
Dane<-Dane[!Dane$y<k1-r,]
Dane<-Dane[!Dane$y>k3+r,]
str(Dane)

k<-quantile(Dane$x1)
k1<-k[2]
k3<-k[4]
r<-3*(k3-k1)/2
Dane<-Dane[!Dane$x1<k1-r,]
Dane<-Dane[!Dane$x1>k3+r,]

#Dane<-Dane[!(Dane$x1>1500),]

#statystyki po oczyszczeniu danych
summary(Dane)

sd#wsp??czynniki zmienno?ci
wz<-function(x){
  sd(x)/mean(x)
}
wz(Dane$x1)
wz(Dane$x2)
wz(Dane$x3) 
wz(Dane$x4)
wz(Dane$x5)
wz(Dane$x6.1)
wz(Dane$x6.2)
wz(Dane$x7)  
wz(Dane$x8)
wz(Dane$x9.1)
wz(dane$x9.2)
wz(Dane$x10)
wz(Dane$x11)
wz(Dane$x12)
wz(Dane$x13)
wz(Dane$x14)
wz(Dane$x15)


model<- lm(y~., data=Dane)
summary(model)
shapiro.test(model$residuals)  #mamy rozk?ad normalny

#usuwanie danych odstaj?cych dla reszt
kDane<-data.frame(Dane, reszty=model$residuals)
k<-quantile(kDane$reszty)
k1<-k[2]
k3<-k[4]
r<-3*(k3-k1)/2
str(Dane)
str(kDane)
Dane<-Dane[!kDane$reszty<(k1-r),]
Dane<-Dane[!kDane$reszty>(k3+r),]
str(Dane)



cormat <- round(cor(Dane),2)
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)
show(melted_cormat)
library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

cor(Dane$y,Dane)

#usuwamy X7
Dane<-Dane[,c( -9,-11)]

str(Dane)

nrow(Dane)*.9
Dane0<-Dane
Dane <- Dane[-sample(1:nrow(Dane), 4), ]
model<- lm(y ~., data=Dane)
summary(model)
shapiro.test(model$residuals) #rozk?ad normlany jest
str(Dane)
AIC(model)
BIC(model)

#HELLWIG
#corrVector - wektor korelacji Y z X, corrMatrix - macierz korelacji miedzy X
hellwig = function(corrYX = NULL, corrX = NULL) {
  
  validateVector = function(vector) {
    if (length(vector) == 0) {
      stop("Vector cannot be empty")
    }
    
    if (!is.numeric(vector)) {
      stop ("Vector must contain numeric values")
    }
    
    numberOfRows = nrow(vector)
    numberOfColumns = ncol(vector)
    hasDimmensions = as.logical(numberOfRows) && as.logical(numberOfColumns)
    
    if (hasDimmensions && !is.na(hasDimmensions)) {
      if (numberOfRows != 1 || numberOfColumns != 1) {
        warning("Matrix was passed as a value instead of vector and will be transformed into a vector")
      }
    }
  }
  
  validateDiagonalMatrix = function(matrix) {
    if (length(matrix) == 0) {
      stop("Matrix cannot be empty")
    }
    
    if (!is.numeric(matrix)) {
      stop ("Matrix must contain numeric values")
    }
    
    numberOfRows = nrow(matrix)
    numberOfColumns = ncol(matrix)
    hasDimmensions = as.logical(numberOfRows) && as.logical(numberOfColumns)
    
    if (is.na(hasDimmensions)) {
      stop("Value passed as a function argument is not a matrix")
    }
    
    if (numberOfRows != numberOfColumns) {
      stop("Number of columns and rows must be equal in square matrix")
    }
  }
  
  createCorrDataList = function(vector, matrix) {
    data = list();
    
    data[['corrVector']] = as.vector(vector)
    data[['corrMatrix']] = as.matrix(matrix)
    
    return(data)
  }
  
  extractDataFromMatrix = function(matrix) {
    vector = matrix[1, -1]
    matrix = matrix[-1, -1]
    data = createCorrDataList(vector, matrix)
    
    return(data)
  }
  
  prepareData = function(corrYX = NULL, corrX = NULL) {
    if(is.null(corrYX)) {
      stop("There are no data passed as an argument")
    } else if(is.null(corrX)) {
      matrix = as.matrix(corrYX)
      validateDiagonalMatrix(matrix)
      
      data = extractDataFromMatrix(matrix)
      
      return(data)
    } else {
      vector = as.vector(corrYX)
      validateVector(corrYX)
      matrix = as.matrix(corrX)
      validateDiagonalMatrix(corrX)
      if(length(vector) != ncol(matrix)) {
        stop("Number of variables in correlation vector is different than number of variables in matrix")
      }
      
      data = createCorrDataList(vector, matrix)
      
      return(data)
    }
  }
  
  #Generowanie wszystkich kombinacji zmiennych
  generateVariablesCombinations = function(numberOfVariables) {
    combinations = list();
    
    for (combinationSize in 1:numberOfVariables) {
      combination = combn(numberOfVariables, combinationSize)
      numberOfCombinations = ncol(combination)
      
      for(combinationIndex in 1:numberOfCombinations) {
        combinations = c(combinations, list(combination[,combinationIndex]))
      }
    }
    
    return(combinations)
  }
  
  #Indywidualna pojemno?????? no???nik???w informacji (hkj)
  calculateIndividualCapacity = function(corrVector, corrMatrix) {
    return((corrVector^2)/sum(abs(corrMatrix)))
  }
  
  #Pojemno?????? integralna kombinacji no???nik???w informacji (Hk)
  calculateIntegralCapacity = function(corrVector, corrMatrix, combination) {
    integralCapacity = 0
    combinationSize = length(combination)
    
    for(combinationElement in 1:combinationSize) {
      variableNumber = combination[combinationElement]
      corrY = corrVector[variableNumber]
      corrX = corrMatrix[variableNumber, combination]
      
      individalCapacity = calculateIndividualCapacity(corrY, corrX)
      
      integralCapacity = integralCapacity + individalCapacity
    }
    
    
    return (integralCapacity)
  }
  
  calculateIntegralCapacities = function(corrVector, corrMatrix, combinations) {
    integralCapacities = NULL
    numberOfCombinations = length(combinations)
    
    for(combinationNumber in 1:numberOfCombinations) {
      combination = combinations[[combinationNumber]]
      
      #Pojemno?????? integralna kombinacji no???nik???w informacji (Hk)
      integralCapacity = calculateIntegralCapacity(corrVector, corrMatrix, combination)
      
      integralCapacities = c(integralCapacities, integralCapacity)
    }
    
    return(integralCapacities)
  }
  
  findBestCombinationIndex =  function(integralCapacities) {
    return(which(integralCapacities == max(integralCapacities)))
  }
  
  createBestCombinationData = function(corrMatrix, combinations, integralCapacities) {
    data = list()
    bestCombinationIndex = findBestCombinationIndex(integralCapacities)
    bestCombination = combinations[[bestCombinationIndex]]
    
    if(is.null(colnames(corrMatrix))) {
      data[["combination"]] = bestCombination
    } else {
      corrMatrixVariablesNames = colnames(corrMatrix)
      
      data[["combination"]] = corrMatrixVariablesNames[bestCombination]
    }
    
    data[["integralCapacity"]] = integralCapacities[bestCombinationIndex]
    
    return(data)
  }
  
  result = tryCatch({
    data = prepareData(corrYX, corrX)
    
    corrVector = data$corrVector
    corrMatrix = data$corrMatrix
    
    numberOfVariables = length(corrVector)
    
    #Generowanie kombinacji zmiennych
    combinations = generateVariablesCombinations(numberOfVariables)
    
    #Obliczanie pojemno???ci integralnej dla ka???dej kombinacji
    integralCapacities = calculateIntegralCapacities(corrVector, corrMatrix, combinations)
    
    bestCombination = createBestCombinationData(corrMatrix, combinations, integralCapacities)
    
    return(bestCombination)
    
  }, error = function(err) {
    message(err)
    
    return (NA)
  }, warning = function(warn) {
    message(warn)
    
    return (NULL)
  })
}

summary(Dane)
R0 = c(cor(Dane$y, Dane[2:17]))
R = cor(Dane[2:17])
hellwig(R0, R)

#model na podstawie Hellwiga
model_hell<-lm(y ~x1+x3+x6.1+x10+x12+x16, data=Dane)
shapiro.test(model_hell$residuals)
summary(model_hell)
AIC(model_hell)
BIC(model_hell)

#metoda krokowa
step(model)
#model inny ni? Hellwig
#Dane$x1<-log(Dane$x1)
model_step<-lm(y ~ x1+x2+x3+x5+x6.1+x6.2+x10+x12+x16, data=Dane)
shapiro.test(model_step$residuals) #jest normlany
summary(model_step)
AIC(model_step)
BIC(model_step)



Dane4<-Dane[c("y",  "x1","x2","x3", "x6.1" ,"x10","x12","x16")]

head(Dane4)
#kt?ry model bierzemy:
model4<-model_step

#sprawdzamy koincydencje
summary(model4)
cor(Dane4$y, Dane4) 
#model jest koincydentny 

str(Dane4)
model4<-lm(y ~., data=Dane4)

#wsp??liniowo??
vif(model4)
#nie jest >10 - nie wyst?puj? wsp??liniowo??

ZwrocKatalizatory = function(R,R0)
{
  ##regularna para korelacyjna
  ##zamiana wartosci ujemnych na dodatnie
  Rprim = R
  R0prim = R0
  wiersze = c()
  for (i in 1:length(R0prim[,1])) {
    if (R0prim[i,] < 0) {
      R0prim[i,] = -R0prim[i,]
      Rprim[i,] = -Rprim[i,]
      Rprim[i,i] = 1
    }
  }
  for (i in 1:length(R0prim[,1])) {
    if (R0[i,] < 0) {
      Rprim[,i] = -Rprim[,i]
      Rprim[i,i] = 1
    }
  }
  
  
  ##uporzadkowanie zmiennych
  R0bis = sort(R0prim[,1])
  R0bis = as.data.frame(R0bis)
  Rbis = Rprim
  indeksy = data.frame(seq(1,length(R0bis[,1]),by = 1),rep(-1,length(R0bis)))
  for (j in 1:length(R0prim[,1])) {
    for (i in 1:length(R0bis[,1])) {
      if (R0prim[j,] == R0bis[i,]) {
        indeksy[j,2] = i 
      } 
    }
  }
  Rbis[indeksy[1,2],indeksy[2,2]]
  Rprim[indeksy[1,1],indeksy[2,1]]
  kontrolna = 0
  for (i in 1:length(R0bis[,1])) {
    kontrolna = kontrolna +1
    j = kontrolna
    while (j <= length(R0bis[,1])) {
      Rbis[indeksy[i,2],indeksy[j,2]] = Rprim[indeksy[i,1],indeksy[j,1]]
      Rbis[indeksy[j,2],indeksy[i,2]] = Rprim[indeksy[j,1],indeksy[i,1]]
      j = j+1
    }
  }
  print("R0 regularne")
  print(R0bis)
  print("R regularne")
  print(Rbis)
  #utworzenie macierzy Q
  Q = Rbis
  for (i in 1:length(R0bis[,1])) {
    Q[i,] = rep(-5,length(R0bis[,1]))
  }
  
  kontrolna = 0
  for (i in 1:length(R0bis[,1])) {
    kontrolna = kontrolna +1
    j = kontrolna
    while (j <= length(R0bis[,1])) {
      Q[i,j] = R0bis[i,]/R0bis[j,]
      j = j+1
    }
  }
  #znajdowanie elementow Rbis< 0 lub wiekszych niz Q
  RwiekszeNizQ = Q
  RMniejszeOdZera = Q
  kontrolna = 0
  for (i in 1:length(R0bis[,1])) {
    kontrolna = kontrolna +1
    j = kontrolna
    while (j <= length(R0bis[,1])) {
      if(Rbis[i,j] < 0){ RMniejszeOdZera[i,j] = T }
      else{ RMniejszeOdZera[i,j] = F  }
      if(Rbis[i,j] > Q[i,j]){ RwiekszeNizQ[i,j] = T }
      else{ RwiekszeNizQ[i,j] = F  }
      j = j+1
    }
  }
  #??czenie elementow w 1 dataframe
  PotencjalneKatalizatory = Q
  kontrolna = 0
  for (i in 1:length(R0bis[,1])) {
    kontrolna = kontrolna +1
    j = kontrolna
    while (j <= length(R0bis[,1])) {
      if (RwiekszeNizQ[i,j] == F & RMniejszeOdZera[i,j] == F){ PotencjalneKatalizatory[i,j] = F}
      else{ PotencjalneKatalizatory[i,j] = T }
      j = j+1
    }
  }
  
  #znadujdowanie katalizatorow
  kontrolna = 0
  katalizatory = c()
  for (i in 1:length(R0bis[,1])) {
    kontrolna = kontrolna +1
    j = kontrolna
    while (j <= length(R0bis[,1])) {
      if (PotencjalneKatalizatory[i,j] == T )
      {
        if(i<j) {wieksza = i}
        else {wieksza = j}
        for (p in 1:length(indeksy[,2])) {
          if(indeksy[p,2] == wieksza) {katalizatory = c(katalizatory,rownames(R0prim)[p])}
        }
      }
      j = j+1
    }
  }
  
  katalizatory = unique(katalizatory)
  katalizatory = sort(katalizatory)
  katalizatory = as.data.frame(katalizatory)
  print("katalizatory")
  print(katalizatory)
  return(katalizatory)
}

R = cor(Dane4[-1],method="pearson")
R0 = cor(Dane4[-1],y = Dane4[1],method="pearson")
ZwrocKatalizatory(R, R0)
#liczenie nat??enia katalizy

integralCapacity=data.frame()


integralCapacity[1,1]<-cor(Dane4$y, Dane4$x1)^2/(sum(abs(cor(Dane4$x1,Dane4[c(-1)]))))
integralCapacity[2,1]<-cor(Dane4$y, Dane4$x2)^2/(sum(abs(cor(Dane4$x2,Dane4[c(-1)]))))
integralCapacity[3,1]<-cor(Dane4$y, Dane4$x3)^2/(sum(abs(cor(Dane4$x3,Dane4[c(-1)]))))
#integralCapacity[4,1]<-cor(Dane4$y, Dane4$x4)^2/(sum(abs(cor(Dane4$x4,Dane4[c(-1)]))))
#integralCapacity[4,1]<-cor(Dane4$y, Dane4$x5)^2/(sum(abs(cor(Dane4$x5,Dane4[c(-1)]))))
integralCapacity[4,1]<-cor(Dane4$y, Dane4$x6.1)^2/(sum(abs(cor(Dane4$x6.1,Dane4[c(-1)]))))
#integralCapacity[6,1]<-cor(Dane4$y, Dane4$x6.2)^2/(sum(abs(cor(Dane4$x6.2,Dane4[c(-1)]))))
#integralCapacity[5,1]<-cor(Dane4$y, Dane4$x9.1)^2/(sum(abs(cor(Dane4$x9.1,Dane4[c(-1)]))))
#integralCapacity[8,1]<-cor(Dane4$y, Dane4$x9.2)^2/(sum(abs(cor(Dane4$x9.2,Dane4[c(-1)]))))
integralCapacity[5,1]<-cor(Dane4$y, Dane4$x10)^2/(sum(abs(cor(Dane4$x10,Dane4[c(-1)]))))
#integralCapacity[6,1]<-cor(Dane4$y, Dane4$x11)^2/(sum(abs(cor(Dane4$x11,Dane4[c(-1)]))))
integralCapacity[6,1]<-cor(Dane4$y, Dane4$x12)^2/(sum(abs(cor(Dane4$x12,Dane4[c(-1)]))))
#integralCapacity[9,1]<-cor(Dane4$y, Dane4$x13)^2/(sum(abs(cor(Dane4$x13,Dane4[c(-1)]))))
#integralCapacity[11,1]<-cor(Dane4$y, Dane4$x14)^2/(sum(abs(cor(Dane4$x14,Dane4[c(-1)]))))
#integralCapacity[7,1]<-cor(Dane4$y, Dane4$x15)^2/(sum(abs(cor(Dane4$x15,Dane4[c(-1)]))))
integralCapacity[7,1]<-cor(Dane4$y, Dane4$x16)^2/(sum(abs(cor(Dane4$x16,Dane4[c(-1)]))))


natezenieKatalizy<-summary(model4)$r.squared-sum(integralCapacity)
show(natezenieKatalizy)

wzgledneNatezenie<-natezenieKatalizy/summary(model4)$r.squared*100
show(wzgledneNatezenie)


#usuwamy katalizatory x5
head(Dane4)
Dane4<-Dane4[,-4]
model4<-lm(y ~., data=Dane4)
summary(model4)
shapiro.test(model4$residuals)


#sprawdzamy liniowosc i losowosc >0.05
randtests::runs.test(model$residuals)
resettest(model4)

plot(model4$residuals)


#stabilnosc parametrow (>0.05)
sctest(model4, method="chow")

#istotno?? r^2
waldtest(model4, test=c("F", "Chisq"))

#HETEROSKEDASTYCZNO?? (>0.05)
bptest(model4)

# test white'a
m <- model
data <- Dane4
u2 <- m$residuals^2
y <- fitted(m)
Ru2<- summary(lm(u2 ~ y + I(y^2)))$r.squared
LM <- nrow(data)*Ru2
p.value <- 1-pchisq(LM, 2)
p.value

shapiro.test(model4$residuals)
# wyst?puje

dwtest(model)
plot(Dane4$y, model4$residuals)
#ostateczny model
model<-model4
Dane<-Dane4
summary(model)

AIC(model)
BIC(model)

#PROGNOZA EX POST

Dane.1<-Dane0[!(Dane0$y %in% Dane4$y),]



prog<-coefficients(model)[1]+coefficients(model)['x1']*Dane.1$x1
                            +coefficients(model)['x2']*Dane.1$x2
                            +coefficients(model)['x3']*Dane.1$x3
                            #+coefficients(model)['x4']*Dane.1$x4
                            #+coefficients(model)['x5']*Dane.1$x5
                            +coefficients(model)['x6.1']*Dane.1$x6.1
                            #+coefficients(model)['x6.2']*Dane.1$x6.2
                            #+coefficients(model)['x7']*Dane.1$x7
                            #+coefficients(model)['x8']*Dane.1$x8
                            #+coefficients(model)['x9.1']*Dane.1$x9.1
                            +coefficients(model)['x10']*Dane.1$x10
                            #+coefficients(model)['x11']*Dane.1$x11
                            +coefficients(model)['x12']*Dane.1$x12
                            #+coefficients(model)['x13']*Dane.1$x13
                            #+coefficients(model)['x14']*Dane.1$x14
                            #+coefficients(model)['x15']*Dane.1$x15
                            +coefficients(model)['x16']*Dane.1$x16
                            


#metoda model nieliniowy
modelNL<- nls(y~a*exp(i*log(x1))+b*exp(x2)+c*x3+d*x6.1+e*x10+f*x12+g*x16+h, data=Dane, start = c(a=0.5, b=80,c=160,d=-74,e=-40,f=80,g=90,h=100,i=0.5))
summary(modelNL)                           
predict(modelNL)  

plot(Dane$x2,Dane$y)
lines(Dane$x2,predict(modelNL),lty=2,col="red",lwd=3)

roznica2<-Dane$y-predict(modelNL)
err<-sqrt(mean(roznica2^2))
summary(modelNL)

roznica<-Dane.1$y-prog

ME<-mean(roznica)
MAE<-mean(abs(roznica))
MSE<-mean(roznica*roznica)
RMSE<-sqrt(MSE)
MAPE<-mean(abs(roznica/Dane.1$y))*100

show(prog)
plot(Dane$x1, Dane$y)
abline(coefficients(model)[1],coefficients(model)['x1'])

plot(Dane$x2,Dane$y)



proba_mod<-nls(y~(exp(a+b*x2+c*x3+d*x6.1+e*x10+f*x12+g*x16))/(1-exp(a+b*x2+c*x3+d*x6.1+e*x10+f*x12+g*x16)),start=c(a=10, b=10,c=10,d=10,e=10,f=10,g=10),data=Dane)



