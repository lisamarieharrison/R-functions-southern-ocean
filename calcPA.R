#function to print observed vs fitted presence/absence and sensitivity/specificity
#author: Lisa-Marie Harrison
#date: 11/03/2016

library(caret)

calcPA <- function (model, reference, data) {
  
  #model: model object
  #reference: reference whale_pa data set
  
  if (class(model)[1] == "hurdle") {
    
    ilogit <- function(x) 1/(1+exp(-x))
    presence_absence <- round(ilogit(data$krill * model$coefficients$zero[1]))
    
    
  } else {
    
    presence_absence <- round(fitted(model))
    presence_absence[presence_absence > 0] <- 1
    
  }
  
  contingency_table <- table(reference, presence_absence, dnn = c("reference", "fitted"))
  
  print(contingency_table)
  
  sens <- round(sensitivity(as.factor(presence_absence), as.factor(reference)), 2)
  spec <- round(specificity(as.factor(presence_absence), as.factor(reference)), 2)
  
  cat("\n", paste("Sensitivity =", sens))
  cat("\n", paste("Specificity =", spec))
  
  return(list("Sensitivity" = sens, "Specificity" = spec))
  
}