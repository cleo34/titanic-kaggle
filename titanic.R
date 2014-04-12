setwd("/home/ales/docs/Dropbox/Docs/dev/titanic-kaggle")

## Read data
titanic.full  <- read.csv("train.csv")
titanic.predict <- read.csv("test.csv")

n <- nrow(titanic.full)
nex <- 200
set.seed(1234)
subs <- split(titanic.full, sample(rep(1:2, times=c(n-nex, nex))))
titanic.train <- subs[[1]]
titanic.test  <- subs[[2]]

##
## Summary and visualization
##
library(plyr)
library(ggplot2)

## sex and pclass are valuable
ddply(.data=titanic.train, .(pclass,sex), summarize,
      total=length(pclass),
      survived=sum(survived),
      percentage=survived/total)

## fare can be used as a feature within pclass
pclassrange <- with(titanic.train, min(pclass):max(pclass))

sapply(pclassrange, function(x) with(titanic.train, summary(fare[pclass==x])))

# ggplot(titanic.train, aes(x=fare)) + geom_histogram(binwidth=25) + facet_grid(.~pclass, scales="free_x")

df <- ddply(titanic.train, .(pclass,survived), summarize,
                qL = quantile(fare,0.05),
                q1 = quantile(fare,0.20),
                q2 = quantile(fare,0.40),
                q3 = quantile(fare,0.60),
                q4 = quantile(fare,0.80),
                qR = quantile(fare,0.95))
# ggplot(df, aes(ymin = pclass-0.25, ymax = pclass+0.25, title="Fare by class")) + 
#   scale_y_continuous(breaks = pclassrange) +
#   geom_rect(aes(xmin=qL,xmax=qR), fill = "white",     colour = "black") + 
#   geom_rect(aes(xmin=q1,xmax=q4), fill = "lightblue", colour = "black") +
#   geom_rect(aes(xmin=q2,xmax=q3), fill = "blue",      colour = "black") +
#   facet_grid(survived~pclass, scales="free")

# Need missing fare modelling
titanic.predict$fare[is.na(titanic.predict$fare)] <- median(titanic.predict$fare[titanic.predict$pclass==3 & !is.na(titanic.predict$fare)])
  
## Constructing stratas
getStrata <- function(pc, df){
  ret <- with(df, 
              cut(fare[pclass==pc],
              breaks=quantile(fare[pclass==pc], probs=c(0.00,0.25,0.75,1.00)),
              labels=F,
              include.lowest=T))
  return (ret)
}
sapply(pclassrange, function(x){
  titanic.train$fare.strata[titanic.train$pclass==x] <<- getStrata(x, titanic.train)
})
sapply(pclassrange, function(x){
  titanic.test$fare.strata[titanic.test$pclass==x] <<- getStrata(x, titanic.test)
})
sapply(pclassrange, function(x){
  titanic.full$fare.strata[titanic.full$pclass==x] <<- getStrata(x, titanic.full)
})

#
sapply(pclassrange, function(x){
  titanic.predict$fare.strata[titanic.predict$pclass==x] <<- getStrata(x, titanic.predict)
})

df <- ddply(.data=titanic.train, .(pclass,fare.strata), summarize,
      total=length(survived),
      survived=sum(survived),
      percentage=survived/total)
# ggplot(df, aes(x=fare.strata, y=percentage, title="Percentage of survived by strata in pclass")) +
#   geom_bar(stat="identity") +
#   facet_grid(~pclass, scale="free")


## missing value for age
sapply(titanic.train, function(x) sum(is.na(x)))

subs <- split(titanic.train, is.na(titanic.train$age))
titanic.train.age <- subs[[1]]
titanic.train.not.age <- subs[[2]]

subs <- split(titanic.test, is.na(titanic.test$age))
titanic.test.age <- subs[[1]]
titanic.test.not.age <- subs[[2]]

source("age.R")

model.age <- train.age.model(titanic.train.age)
summary(model.age)

page <- predict.age.model(titanic.test.age, model.age)
cor(titanic.test.age$age, page)

## set ages
titanic.train$age[is.na(titanic.train$age)] <- predict.age.model(subset(titanic.train, is.na(titanic.train$age)), model.age)

titanic.test$age[is.na(titanic.test$age)] <- predict.age.model(subset(titanic.test, is.na(titanic.test$age)), model.age)

titanic.predict$age[is.na(titanic.predict$age)] <- predict.age.model(subset(titanic.predict, is.na(titanic.predict$age)), model.age)

summary(titanic.train$age)



##
## modelling
##
library(party)
formula1 <- as.formula(as.factor(survived) ~ pclass + sex + fare + age + sibsp)
formula2 <- as.formula(           survived ~ pclass + sex + fare + age + sibsp)
results <- NULL
ps <- NULL
NT <- 1000
##
## ctree
##
titanic.ctree <- ctree(formula1, data=titanic.train)
# plot(titanic.ctree)
ps$ctree <- as.numeric(predict(titanic.ctree, newdata=titanic.test))-1
tb <- table(titanic.test$survived, ps$ctree)
# tb
results$ctree <- (tb[1,1]+tb[2,2])/sum(tb)

##
## cforest
##
titanic.cforest <- cforest(formula1, data=titanic.train, 
                           control=cforest_classical(ntree=NT, trace=F))
ps$cforest <- as.numeric(predict(titanic.cforest, newdata=titanic.test))-1
tb <- table(titanic.test$survived, ps$cforest)
# tb
results$cforest <- (tb[1,1]+tb[2,2])/sum(tb)

##
## randomForest
##
library(randomForest)

titanic.rf <- randomForest(formula1, data=titanic.train, ntree=NT, importance=T)
# importance(titanic.rf)
ps$randomforest <- as.numeric(predict(titanic.rf, newdata=titanic.test))-1
tb <- table(titanic.test$survived, ps$randomforest)
# tb
results$randomforest <- (tb[1,1]+tb[2,2])/sum(tb)

##
## gbm
##
library(gbm)

titanic.gbm <- gbm(formula2, data=titanic.train, n.trees=NT, interaction.depth=2)

ps$gbm <- predict(titanic.gbm, newdata=titanic.test, type="response", n.trees=NT)
ps$gbm <- round(ps$gbm, 0)
tb <- table(titanic.test$survived, ps$gbm)
# tb
results$gbm <- (tb[1,1]+tb[2,2])/sum(tb)


##
## neural network
##
library(neuralnet)

titanic.train.nn <- titanic.train
titanic.train.nn$sex <- as.numeric(titanic.train.nn$sex)-1
## LONG RUN
titanic.nn <- neuralnet(formula2, data=titanic.train.nn, hidden=c(8), stepmax=1e9, threshold=0.1)
## ********
plot(titanic.nn)

titanic.test.nn <- titanic.test
titanic.test.nn$sex <- as.numeric(titanic.test.nn$sex)-1

ps$nn <- compute(titanic.nn, titanic.test.nn[,c(2,4,9,5,6)])$net.result
ps$nn <- round(ps$nn, 0)
ps$nn[ps$nn<0] <- 0
ps$nn[ps$nn>1] <- 1
tb <- table(titanic.test$survived, ps$nn)
# tb
results$nn <- (tb[1,1]+tb[2,2])/sum(tb)

##
## SVM
##
library(e1071)
formula3 <- as.factor(survived) ~ pclass + sex + as.factor(fare.strata) + age
tune <- tune.svm(formula3, data=titanic.train, gamma=10^(-4:-1), cost=10^(1:4))
# summary(tune)
tune$best.parameters

titanic.svm <- svm(formula3, 
               data=titanic.train, 
               type="C-classification", 
               kernel="linear", 
               probability=T, 
               gamma=tune$best.parameters$gamma, 
               cost=tune$best.parameters$cost)
ps$svm <- as.numeric(predict(titanic.svm, newdata=titanic.test))-1
tb <- table(titanic.test$survived, ps$svm)
# tb
results$svm <- (tb[1,1]+tb[2,2])/sum(tb)
results$svm

#
# show results
#
as.data.frame(results)

psr <- as.data.frame(ps)
psr$MEDIAN <- round(apply(psr, 1, median), 0)
psr$RES <- as.numeric(titanic.test$survived)

psr$ALL1 <- apply(psr[,1:6],1,mean)==1
psr$ALL0 <- apply(psr[,1:6],1,mean)==0

# Correct median(?)
psr$MEDIAN[!psr$ALL0 & psr$MEDIAN==0] <- psr$randomforest[!psr$ALL0 & psr$MEDIAN==0]

head(psr, 10)

tb <- table(psr$MEDIAN, psr$RES)
tb
(tb[1,1]+tb[2,2])/sum(tb)

# after compute analysis

subset(psr, !psr$ALL1 & psr$MEDIAN==1 & psr$RES!=1) # FP
subset(psr, !psr$ALL0 & psr$MEDIAN==0 & psr$RES!=0) # FN

subset(psr, psr$MEDIAN!=psr$RES & !psr$ALL1 & !psr$ALL0)

#
#
#

sapply(titanic.predict, function(x) sum(is.na(x)))

ans <- as.numeric(predict(titanic.rf, newdata=titanic.predict))-1
write.csv(ans, file="submission02.csv", row.names=F)

f1 <- read.csv("submission01.csv")
f2 <- read.csv("submission02.csv")

df <- cbind(f1, f2)
subset(df, df[,1]!=df[,2])
