#calculates marginal and conditional R-squared values for asreml mixed model object
#author: Lisa-Marie Harrison
#date: 3/8/2015

calcRsquared <- function (obj, varStruct, rand=NULL, data) {
  
  #obj: asreml model object
  #varStruct: Boolean specifying if model contains a correlation structure or not
  #rand: optional vector of strings containing names of random effects (splines are considered fixed so do not include)
  #return: a list containing the marginal and conditional R-squared values
  
  #Get design matrix of fixed effects from model
  Fmat <- model.matrix(eval(obj$fixed.formula)[-2], data)
  
  #Get variance of fixed effects by multiplying coefficients by design matrix
  VarF <- sum(var(as.vector(rev(obj$coefficients$fixed) %*% t(Fmat))), summary(obj)$varcomp[grep("spl", rownames(summary(obj)$varcomp)), 2])
  
  if(varStruct) {
    #if variance structure present, add to VarF
    #not sure if correct but the model explains this variance so it should be included in R-squared
    #added to VarF because it isn't a random effect
    VarF <- sum(VarF, summary(obj)$varcomp[(grep("R!variance", rownames(summary(obj)$varcomp)) + 1):nrow(summary(obj)$varcomp), 2])
  }
  
  if (is.null(rand)) {
    VarRand <- 0
  } else {
    #Get variance of random effects by extracting variance components
    VarRand <- sum(summary(obj)$varcomp[grep(rand, rownames(summary(obj)$varcomp)), 2])
  }
  
  
  #Get residual variance
  VarResid <- summary(obj)$varcomp[grep("R!variance", rownames(summary(obj)$varcomp)), 2]
  
  varTotal <- VarF + VarRand + VarResid
  
  #calculate marginal R-squared
  marR2 <- VarF/varTotal
  
  #calculate conditional R-squared
  condR2 <- (VarF + VarRand)/varTotal
  
  if(is.null(rand)) {
    return(list(marginal = marR2))
  } else {
    return(list(marginal = marR2, conditional = condR2))
  }
  
}



