
dane_1 <- read.table(file = "...", 
                     sep=";", dec=";", header=TRUE)
dane_1<-dane_1[,c(-9, -14)]
colnames(dane_1)[1]<-"y"

# RANDOM FOREST

#install.packages("MASS")
#install.packages("randomForest")
library(MASS)
library(randomForest)

set.seed(101)
train = sample(1:nrow(dane_1), 40)

rf = randomForest(y~., data = dane_1, subset = train)
rf

oob.err = double(18)
test.err = double(18)
MAPE = double(18)
for(mtry in 1:18){
  fit = randomForest(y~., data = dane_1, subset=train, mtry=mtry, ntree = 350)
  oob.err[mtry] = fit$mse[350]
  pred = predict(fit, dane_1[-train,])
  test.err[mtry] = with(dane_1[-train,], mean( (y-pred)^2 ))
  MAPE[mtry] = with(dane_1[-train,],mean(abs((y-pred)/y)))
}

matplot(1:mtry, cbind(test.err, oob.err), pch = 23, col = c("red", "blue"), type = "b", ylab="Mean Squared Error")
legend("topright", legend = c("OOB", "Test"), pch = 23, col = c("red", "blue"))

oob.err
test.err
MAPE


# BOOSTING

#install.packages("gbm")
library(gbm)

boost.dane = gbm(y~., data = dane_1[train,], distribution = "gaussian", n.trees = 1000, shrinkage = 0.5, interaction.depth = 4, bag.fraction = 0.8)
summary(boost.dane)

plot(boost.dane,i="x1")
plot(boost.dane,i="x6.1")

n.trees = seq(from = 100, to = 1000, by = 100)
predmat = predict(boost.dane, newdata = dane_1[-train,], n.trees = n.trees)
dim(predmat)

boost.err = with(dane_1[-train,], apply( (predmat - y)^2, 2, mean) )
plot(n.trees, boost.err, pch = 23, ylab = "Mean Squared Error", xlab = "# Trees", main = "Boosting Test Error")
abline(h = min(test.err), col = "red")

boost.MAPE = with(dane_1[-train,],apply(abs((y-predmat)/y),2,mean))
boost.MAPE
boost.err


# CLLASIFICATION TREES

#install.packages("ISLR")
#install.packages("tree")
library(tree)
library(ISLR)


hist(dane_1$y)

High = ifelse(dane_1$y<=450, "No", "Yes")
dane = data.frame(dane_1, High)
tree.class = tree(High~.-y, data=dane)
summary(tree.class)
plot(tree.class)
text(tree.class, pretty = 0, cex = 0.8)
tree.class


set.seed(101)
train=sample(1:nrow(dane), 40)

tree.class2 = tree(High~.-y, dane, subset=train)
plot(tree.class2)
text(tree.class2, pretty=0, cex=0.7)

tree.pred = predict(tree.class2, dane[-train,], type="class")
with(dane[-train,], table(tree.pred, High))

(1+1)/12

cv = cv.tree(tree.class2, FUN = prune.misclass)
cv
plot(cv)

prune = prune.misclass(tree.class2, best = 4)
plot(prune)
text(prune, pretty=0, cex=0.7)

tree.pred = predict(prune, dane[-train,], type="class")
with(dane[-train,], table(tree.pred, High))
(1+1) / 12

# REGGRESSION TREE

install.packages("rpart.plot")
install.packages("caret")
installed.packages("ipred")
install.packages("rsample")
library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)
library(ISLR)


set.seed(101)
split= initial_split(dane_1, prop= .7)
train = training(split)
test = testing(split)

m1 <- rpart(
  formula = y ~ .,
  data    = train,
  method  = "anova"
)
m1
rpart.plot(m1, cex = 0.7)
plotcp(m1)
printcp(m1)

m2 <- rpart(
  formula = y ~ .,
  data    = train,
  method  = "anova", 
  control = list(cp = 0, xval = 5)
)

plotcp(m2)
abline(v = 1, lty = "dashed")

m3 <- rpart(
  formula = y ~ .,
  data    = train,
  method  = "anova", 
  control = list(minsplit = 5, maxdepth = 12, xval = 5)
)

m3$cptable

hyper_grid <- expand.grid(
  minsplit = seq(1, 20, 1),
  maxdepth = seq(1, 16, 1)
)


models <- list()

for (i in 1:nrow(hyper_grid)) {
  
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train a model and store in the list
  models[[i]] <- rpart(
    formula = y ~ .,
    data    = train,
    method  = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

# function to get optimal cp
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)



optimal_tree <- rpart(
  formula = y ~ .,
  data    = train,
  method  = "anova",
  control = list(minsplit = 5, maxdepth = 11, cp = 0.02)
)

pred <- predict(optimal_tree, newdata = test)
RMSE(pred = pred, obs = test$y)

APE<-abs((test$y-pred)/test$y)
APE <- APE[is.finite(APE)]
MAPE_regress_tree<- mean(APE)
MAPE_regress_tree

rpart.plot(optimal_tree)

