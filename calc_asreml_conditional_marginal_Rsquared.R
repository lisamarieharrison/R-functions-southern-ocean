#calculates marginal and conditional R-squared values for asreml mixed model object
#author: Lisa-Marie Harrison
#date: 3/8/2015

calcRsquared <- function (obj, rand) {
  
  #obj: asreml model object
  #rand: vector of strings containing names of random effects (splines are considered fixed so do not include)
  #return: a list containing the marginal and conditional R-squared values
  
  #Get design matrix of fixed effects from model
  Fmat <- model.matrix(eval(obj$fixed.formula)[-2], glm.spl)
  
  #Get variance of fixed effects by multiplying coefficients by design matrix
  VarF <- sum(var(as.vector(rev(obj$coefficients$fixed) %*% t(Fmat))), summary(obj)$varcomp[grep("spl", rownames(summary(obj)$varcomp)), 2])
  
  #Get variance of random effects by extracting variance components
  VarRand <- sum(summary(obj)$varcomp[grep(rand, rownames(summary(obj)$varcomp)), 2])
  
  #Get residual variance
  VarResid <- summary(obj)$varcomp[grep("R!variance", rownames(summary(obj)$varcomp)), 2]
  
  varTotal <- VarF + VarRand + VarResid
  
  #calculate marginal R-squared
  marR2 <- VarF/varTotal
  
  #calculate conditional R-squared
  condR2 <- (VarF + VarRand)/varTotal
  
  return(list(marginal = marR2, conditional = condR2))
  
}



