#creates data for plotting a ROC curve for a binomial glm
#author: Lisa-Marie Harrison
#date: 19/11/2015

rocCurve <- function(model, s, data, print = FALSE) {
  
  #model= binomial glm model from glmer
  #s = threshold (usually 0.5)
  #data = data set with column named 'pa' for presence absence data
  #print= boolean for printing table of observed vs predicted
  
  Y <- na.omit(data)
  S <- predict(model, type = "response")
  Ps <- (S > s)*1
  FP <- sum((Ps == 1)*(Y == 0))/sum(Y == 0)
  TP <- sum((Ps == 1)*(Y == 1))/sum(Y == 1)
  if (print) {
    print(table(Observed = Y, Predicted = Ps))
  }
  vect <- c(FP, TP)
  names(vect) <- c("FPR", "TPR")
  return(vect)
}

