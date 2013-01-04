##
##
##
train.age.model <- function(df){
  stopifnot(is.data.frame(df))
  
  glm(age~pclass:sex+sibsp+fare.strata+as.factor(embarked), data=df)
}

##
##
##
predict.age.model <- function(df, model){
  stopifnot(is.data.frame(df))
  stopifnot(class(model)[1]=="glm")
  
  predict(model, newdata=df)
}