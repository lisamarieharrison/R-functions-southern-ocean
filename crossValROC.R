#function to calculate AUC of ROC for cross validation data
#author: Lisa-Marie Harrison
#date: 1/11/2016

crossValROC <- function(pred, truth) {
  
  #pred = predictions from any model
  #truth = corresponding true values
  
  roc.curve <- function(s, print = FALSE){
  
    Ps <- (pred > s)*1
    FP <- sum((Ps == 1)*(truth == 0))/sum(truth == 0)
    TP <- sum((Ps == 1)*(truth == 1))/sum(truth == 1)
    
    vect <- c(FP,TP)
    names(vect) <- c("FPR", "TPR")
    return(vect)
  }
  
  ROC.curve <- Vectorize(roc.curve)
  M.ROC     <- ROC.curve(seq(0, 1, by = 0.01))
  
  auc <- auc(M.ROC[1,], M.ROC[2,])
  
  return(auc)
  
}

