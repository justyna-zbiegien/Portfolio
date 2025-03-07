rm(list=ls()) 
dev.off()
cat("\014")


library("readxl")
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
#install.packages("factoextra")
#install.packages("FactoMineR")
library(dummy)
library(factoextra)
library(FactoMineR)
library(corrplot)
require(foreign)
require(nnet)
require(reshape2)
require(MASS)
require(Hmisc)
library(data.table)
require(modelsummary)
library(effects)
library(knitr)

#Wczytywanie danych-------------------------------------------


dane_all<-read_excel("C:\\Justyna\\Zadania\\Mgr\\magisterka\\02. Dane\\01. Baza danych\\Baza 2022 SGH Studenci (raw).xlsx")
dane<-dane_all[,c('K4','K5','B1','B2r1','D1', 'D2','B5', 'B9', 'B6_9', 'B8_2', 'O1', 'D3','O4','O5')]
dane[is.na(dane)] = 0

dane<-as.data.frame(dane)

#PCA---------------------------------------------------

danePCA <- dane[,c('B1','D1','D2','B5', 'B9', 'B6_9','B8_2', 'O1', 'D3','O5','B2r1','O4')]


names(danePCA)[names(danePCA) == 'B1'] <- 'syt_fin_do_p'
names(danePCA)[names(danePCA) == 'D1'] <- 'dochod'
names(danePCA)[names(danePCA) == 'D2'] <- 'koszt'
names(danePCA)[names(danePCA) == 'B5'] <- 'kontrola'
names(danePCA)[names(danePCA) == 'B9'] <- 'syt_fin'
names(danePCA)[names(danePCA) == 'B6_9'] <- 'zobowiazania'
names(danePCA)[names(danePCA) == 'B8_2'] <- 'zobo_powod'
names(danePCA)[names(danePCA) == 'O1'] <- 'oszcz'
names(danePCA)[names(danePCA) == 'D3'] <- 'oszczPLN'
names(danePCA)[names(danePCA) == 'O5'] <- 'wpływ_p_oszcz'
names(danePCA)[names(danePCA) == 'B2r1'] <- 'art_spoz'
names(danePCA)[names(danePCA) == 'O4'] <- 'oszcz_zm_w_p'



options(ggrepel.max.overlaps = Inf)

pca<-PCA(danePCA, graph=FALSE)
print(pca)

##Wybór liczby wymiarów i clusterów -----------------------------------------
# Wybranie ile wymiarów  https://towardsdatascience.com/learn-principle-component-analysis-in-r-ddba7c9b1064
# 1. objaśnianie wariancji:
summary(pca)
# 2. współczynnik Kaiser-Guttmana czyli eigenvalue musi być powyżej 1
pca$eig

# 3. test Bartletta (H0: wszystkie współczynniki korelacji są równe zero, chcemy H1 niskie p-value)
bartlett.test(danePCA)
#Wybieramy 5 wymiaróW

var<-get_pca_var(pca)
var
#Scree plot
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 40))
# Coordinates
head(var$coord)
# Cos2: quality on the factor map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)
# korelacje 
corrplot(var$cos2, is.corr=FALSE)

fviz_pca_var(pca, col.var = "res.pca",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(pca, choice = "var", axes = 1:2)
# Color by cos2 values: quality on the factor map
fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)


head(var$contrib, 12)
# Contributions of variables to PC1
fviz_contrib(pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 2, top = 10)
fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

var$contrib
var$cos2

# Create a grouping variable using kmeans
# Create 5 groups of variables (centers = 5)
set.seed(123)
res.km <- kmeans(var$coord, centers=5, nstart=50)
grp <- as.factor(res.km$cluster)

Numer_clustra<-kmeans(pca$ind$coord, centers=5)
Numer_clustra$cluster

c1<-0
c2<-0
c3<-0
c4<-0
c5<-0


for(i in 1:800)
{
  if(Numer_clustra$cluster[i]==1){
    c1<-c1+1
  }else if(Numer_clustra$cluster[i]==2)
    {c2<-c2+1
  }else if(Numer_clustra$cluster[i]==3)
    {c3<-c3+1
  }else if(Numer_clustra$cluster[i]==4)
    {c4<-c4+1
  } else {c5<-c5+1}
}

klastry<-data.frame(c1,c2,c3,c4,c5)
barplot(klastry)

klastry2 <- data.frame(matrix(ncol = 5, nrow = 800))
x <- c("k1", "k2", "k3", "k4","k5")
colnames(klastry2) <- x
klastry2[is.na(klastry2)] <- 0

for(i in 1:800)
{
  if(Numer_clustra$cluster[i]==1){
    klastry2[i,1]=1
  }else if(Numer_clustra$cluster[i]==2)
  {klastry2[i,2]=1
  }else if(Numer_clustra$cluster[i]==3)
  {klastry2[i,3]=1
  }else if(Numer_clustra$cluster[i]==4)
  {klastry2[i,4]=1
  } else {klastry2[i,5]=1}
}


#Model regresji------------------------------

for(i in 1:800)
{
  if(dane$K4[i]== 1||dane$K4[i]==2)
  {
    dane$K4[i]=1
  }else if (dane$K4[i] == 5)
  {
    dane$K4[i]=2
  }else
  {dane$K4[i]=3}
}

components <- cbind(K4 = dane[, "K4"], dane_all$S1, dane_all$M2,dane_all$M4,dane_all$M6) %>%
  as.data.frame()

names(components)[names(components) == 'K4'] <- 'y'
names(components)[names(components) == 'V2'] <- 'plec'
names(components)[names(components) == 'V3'] <- 'stopien'
names(components)[names(components) == 'V4'] <- 'semestr'
names(components)[names(components) == 'V5'] <- 'miejscowosc'


components<-cbind(components,klastry2)
components2<-cbind(K4 = dane[, "K4"], dane_all$S1, dane_all$M2,dane_all$M4,dane_all$M6, Numer_clustra$cluster) %>%
  as.data.frame()

names(components2)[names(components2) == 'K4'] <- 'y'
names(components2)[names(components2) == 'V2'] <- 'plec'
names(components2)[names(components2) == 'V3'] <- 'stopien'
names(components2)[names(components2) == 'V4'] <- 'semestr'
names(components2)[names(components2) == 'V5'] <- 'miejscowosc'

is.factor(components$y)
components$y<-relevel(as.factor(components$y), ref='1')

is.factor(components2$y)
components2$y<-relevel(as.factor(components2$y), ref='1')

model <- polr(y~., data=components, Hess = TRUE)
model2 <- polr(y~., data=components2, Hess = TRUE)
summary(model)
summary(model2)


(ctable <- coef(summary(model)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

# combined table
ctable <- cbind(ctable, "p value" = p)
confint.default(model) 
exp(coef(model))
ci <- confint.default(model)
exp(cbind(OR = coef(model), ci))

modelsummary(list("Kategorie" = model, "Jedna zmienna" = model2), 
             statistic = "{std.error} ({p.value}) {stars}")
pl<-modelsummary(list("Model" = model), 
             statistic = "{std.error} ({p.value}) {stars}")


# Tworzenie zmiennych dummies do OR --------------------------------


components$plec<-as.factor(components$plec)
components$stopien<-as.factor(components$stopien)
components$semestr<-as.factor(components$semestr)
components$miejscowosc<-as.factor(components$miejscowosc)

PLEC<-model.matrix(~ plec - 1, data = components)
STOP<-model.matrix(~ stopien - 1, data = components)
SEM<-model.matrix(~ semestr - 1, data = components)
MSC<-model.matrix(~ miejscowosc - 1, data = components)

#dane_test<-cbind(components$y, PLEC, STOP, SEM, MSC, components$k1, components$k2, components$k3, components$k4)

dane_test<-cbind(components$y, STOP)
dane_test <- dane_test[, -which(names(dane_test) == "stopien1")]


dane_test<-as.data.frame(dane_test)
dane_test$V1<-as.factor(dane_test$V1)

dane_test <- dane_test[, -which(names(dane_test) == "plec3")]
dane_test <- dane_test[, -which(names(dane_test) == "plec3")]


model_test<-polr(V1~., data=dane_test, HESS=TRUE)

COFF<- summary(model_test)$coefficients
coefficients


#wizualiacja modelu---------------------------

fviz_cluster(Numer_clustra, data = pca$ind$coord,
             palette = c("#FFD596", "#E65957", "#9A46FA", "#52ABE3","#66FA8D"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             xlab="Dim 1",
             ylab= "Dim 2"
)


df <- pca$ind$coord[,1:3]

cluster<-Numer_clustra$cluster
df<-cbind(df, cluster)

df<-as.data.frame(df)
is.data.frame(df)

library(plotly)
library(dplyr)
p <- plot_ly(df, x=~Dim.1, y=~Dim.2, 
             z=~Dim.3, color=~cluster) %>%
  add_markers(size=1.5)
print(p)


# test --------------------------

mod = list(
  "POLR" = polr(K4~., data=components, Hess = TRUE))

modelsummary(mod, stars = TRUE)

# niepotrzebne-----------------------------------

# Color variables by groups
fviz_pca_var(pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF", "#22D697", "#CC0033"),
             legend.title = "Cluster")
grp
#cluster1: D1, D2
#cluster2: B1, B5
#cluster3: B6_9, B2r1
#cluster4: O1, B8_2
#cluster5: B9, D3, O4, O5

fviz_pca_ind(pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

