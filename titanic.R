setwd("/home/ales/docs/Dropbox/Docs/dev/titanic-kaggle")

## Read data
titanic.full  <- read.csv("train.csv")
titanic.predict <- read.csv("test.csv")

n <- nrow(titanic.full)
set.seed(1234)
subs <- split(titanic.full, sample(rep(1:2, times=c(n-100, 100))))
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

ggplot(titanic.train, aes(x=fare)) + geom_histogram(binwidth=25) + facet_grid(.~pclass, scales="free_x")

df <- ddply(titanic.train, .(pclass,survived), summarize,
                qL = quantile(fare,0.05),
                q1 = quantile(fare,0.20),
                q2 = quantile(fare,0.40),
                q3 = quantile(fare,0.60),
                q4 = quantile(fare,0.80),
                qR = quantile(fare,0.95))
ggplot(df, aes(ymin = pclass-0.25, ymax = pclass+0.25, title="Fare by class")) + 
  scale_y_continuous(breaks = pclassrange) +
  geom_rect(aes(xmin=qL,xmax=qR), fill = "white",     colour = "black") + 
  geom_rect(aes(xmin=q1,xmax=q4), fill = "lightblue", colour = "black") +
  geom_rect(aes(xmin=q2,xmax=q3), fill = "blue",      colour = "black") +
  facet_grid(survived~pclass, scales="free")

## Constructing stratas
getStrata <- function(pc){
  ret <- with(titanic.train, 
              cut(fare[pclass==pc],
              breaks=quantile(fare[pclass==pc], probs=c(0.00,0.25,0.75,1.00)),
              labels=F,
              include.lowest=T))
  titanic.train$fare.strata[titanic.train$pclass==pc] <<- ret
  return (ret)
}
sapply(pclassrange, getStrata)

df <- ddply(.data=titanic.train, .(pclass,fare.strata), summarize,
      total=length(survived),
      survived=sum(survived),
      percentage=survived/total)
ggplot(df, aes(x=fare.strata, y=percentage, title="Percentage of survived by strata in pclass")) +
  geom_bar(stat="identity") +
  facet_grid(~pclass, scale="free")


## missing value for age
sapply(titanic.train, function(x) sum(is.na(x)))

titanic.full.aged <- subset(titanic.full, !is.na(titanic.full$age))
titanic.full.not.aged <- subset(titanic.full, is.na(titanic.full$age))
model.age <- glm(age~pclass+sex+sibsp+pclass:fare, data=titanic.full.aged)
summary(model.age)
predict(model.age, newdata=titanic.full.not.aged)

