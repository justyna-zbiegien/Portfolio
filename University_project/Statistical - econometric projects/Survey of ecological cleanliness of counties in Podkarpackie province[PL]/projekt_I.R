przeksztalcenie_ilorazowe<- function(wektor){
  wynik=wektor/sqrt(sum(wektor^2))
}
Powiaty <- read.table("powiaty.csv", header=TRUE, sep=";", row.names =1)
wsp_zmiennosci<-function(x){
  sd(x)/mean(x)
}

Powiaty_wz <- matrix(NA, 11, 1)
colnames(Powiaty_wz)<-c("Wsp. Zmiennoœci")
rownames(Powiaty_wz) <- colnames(Powiaty)
for(i in 1:11){
  Powiaty_wz[i] <- format(round(wsp_zmiennosci(Powiaty[,i])*100,2), nsmall=2)
}
Powiaty_wz
Powiaty<-Powiaty[,-2]
Powiaty
powiaty_normalizowane<-matrix(NA,25,10)
for(i in 1:10)
{
  powiaty_normalizowane[,i]=przeksztalcenie_ilorazowe(Powiaty[,i])
}
powiaty_normalizowane
ideal<-c(max(powiaty_normalizowane[,1]),
         max(powiaty_normalizowane[,2]),
         max(powiaty_normalizowane[,3]),
         max(powiaty_normalizowane[,4]),
         max(powiaty_normalizowane[,5]),
         max(powiaty_normalizowane[,6]),
         max(powiaty_normalizowane[,7]),
         max(powiaty_normalizowane[,8]),
         max(powiaty_normalizowane[,9]),
         max(powiaty_normalizowane[,10])
         )
antyideal<-c(min(powiaty_normalizowane[,1]),
             min(powiaty_normalizowane[,2]),
             min(powiaty_normalizowane[,3]),
             min(powiaty_normalizowane[,4]),
             min(powiaty_normalizowane[,5]),
             min(powiaty_normalizowane[,6]),
             min(powiaty_normalizowane[,7]),
             min(powiaty_normalizowane[,8]),
             min(powiaty_normalizowane[,9]),
             min(powiaty_normalizowane[,10])
            )

topsis<-matrix(NA,25,3)

for(i in 1:25){
  OB=powiaty_normalizowane[i,]
  OBid<-rbind(OB, ideal)
  OBanty<-rbind(OB,antyideal)
  topsis[i,1]<-dist(OBid)
  topsis[i,2]<-dist(OBanty)
  topsis[i,3]<-topsis[i,2]/(topsis[i,1]+topsis[i,2])

}
OBid
tabela<-matrix(NA,25,2)
names<-colnames(Powiaty)
names
for(i in 1:25)
{
  tabela[i,1]<-topsis[i,3]
  tabela[i,2]<-row.names(Powiaty[i,])
}
colnames(tabela)<-c("wsp. rankingowy","nazwa")
tabela

tabela_posortowana<-tabela[order(tabela[,1],decreasing = TRUE),]
tabela_posortowana


