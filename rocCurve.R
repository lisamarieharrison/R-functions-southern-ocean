#creates data for plotting a ROC curve for a binomial glm
#author: Lisa-Marie Harrison
#date: 19/11/2015


rocCurve <- function(model, threshold, data, print = FALSE) {
  
  roc.curve <- function(s, print = FALSE){
    
    Y  <- na.omit(data)
    S  <- predict(model, type = "response")
    Ps <- (S > s)*1
    FP <- sum((Ps == 1)*(Y == 0))/sum(Y == 0)
    TP <- sum((Ps == 1)*(Y == 1))/sum(Y == 1)
    
    vect <- c(FP,TP)
    names(vect) <- c("FPR", "TPR")
    
    if(print){
      print(table(Observed = Y,Predicted = Ps))
      print(vect)
    }
    return(vect)
  }
  
  roc.curve(threshold, print)
  ROC.curve <- Vectorize(roc.curve)
  M.ROC     <- ROC.curve(seq(0, 1, by = 0.01))
  
  return(M.ROC)
  
}

