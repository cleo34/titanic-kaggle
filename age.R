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
  
  ret <- predict(model, newdata=df)
  ret[ret<0] <- 0.5
  
  return (ret)
}