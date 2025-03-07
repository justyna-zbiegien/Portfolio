library(foreign)
library(plm)
##############################
Panel3 <- read.csv("C:/Justyna/Zadania/Mgr/sem III/E_Panelowa/projekt/dane_jz107961_mr76692.csv")
View(Panel3)

gdp <- Panel3$loggdppercapita
socialsup <- Panel3$socialsupport
freedom <- Panel3$freedomtomakelifechoices
generos <- Panel3$generosity
corrupt <- Panel3$perceptionsofcorruption
positive <- Panel3$positiveaffect
negative <- Panel3$negativeaffect
democr <- Panel3$democraticquality
delivery <- Panel3$deliveryquality


Y <- cbind(Panel3$lifeladder)
X <- cbind(gdp, socialsup, freedom, generos, corrupt, positive, negative, democr, delivery)

# Set data as panel data
pdata <- pdata.frame (Panel3, index=c("country","year"))
# Descriptive statistics
summary(Y)
summary(X)
# Pooled OLS estimator
pooling <- plm(Y ~ X, data=pdata, model= "pooling")
summary(pooling)
# Between estimator
between <- plm(Y ~ X, data=pdata, model= "between")
summary(between)
# First differences estimator
firstdiff <- plm(Y ~ X, data=pdata, model= "fd")
summary(firstdiff)
# Fixed effects or within estimator
fixed <- plm(Y ~ X, data=pdata, model= "within")
summary(fixed)
# Random effects estimator
random <- plm(Y ~ X, data=pdata, model= "random")
summary(random)

# LM test for random effectsversus OLS
plmtest(pooling)

# LM test for fixed effects versus OLS
pFtest(fixed, pooling)
# Hausman test for fixed versus random effects model
phtest(random, fixed)



#### usuwanie zmiennych nieistotnych ####

Xpool <- cbind(gdp, socialsup, freedom, generos, corrupt, positive, delivery)
pooling2 <- plm(Y ~ Xpool, data=pdata, model= "pooling")
summary(pooling2)

# Between estimator
Xbeet <- cbind(gdp, socialsup, corrupt, positive)
between2 <- plm(Y ~ Xbeet, data=pdata, model= "between")
summary(between2)

# First differences estimator
Xfd <- cbind(gdp, socialsup, freedom, generos, corrupt, positive, negative)
firstdiff2 <- plm(Y ~ Xfd, data=pdata, model= "fd")
summary(firstdiff2)

# Fixed effects or within estimator
Xfe <- cbind(gdp, socialsup, freedom, corrupt, positive, negative, delivery)
fixed2 <- plm(Y ~ Xfe, data=pdata, model= "within")
summary(fixed2)

# Random effects estimator
Xre <- cbind(gdp, socialsup, freedom, corrupt, positive, negative)
random2 <- plm(Y ~ Xre, data=pdata, model= "random")
summary(random2)


# LM test for random effectsversus OLS
plmtest(pooling2)

# LM test for fixed effects versus OLS
pFtest(fixed2, pooling2)
# Hausman test for fixed versus random effects model
phtest(random2, fixed2)


ef_indyw <- fixef(fixed) # Display the fixed effects (constants for each country)
View(ef_indyw)
