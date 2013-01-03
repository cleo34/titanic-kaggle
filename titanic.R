setwd("/home/ales/docs/Dropbox/Docs/dev/titanic-kaggle")

# Read data
titanic.train <- read.csv("train.csv")
titanic.test  <- read.csv("test.csv")
attach(titanic.train)

##
## Summarizing
##
library(plyr)

## sex and pclass are valuable
ddply(titanic.train, .(pclass,sex), summarize,
      total=length(pclass),
      survived=sum(survived),
      percentage=survived/total)

## fare can be used as a feature within pclass
aggregate(fare~pclass, titanic.train, mean)
sapply(sort(unique(pclass)), function(x) summary(fare[pclass==x]))
