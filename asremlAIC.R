asremlAIC <- function(obj) {
  
  #calculates AIC for an asreml model
  #obj = asreml model object
  #returns list containing log likelihood, number of parameters and AIC
  
  l <- obj$logl 
  K <- length(obj$gammas) 
  AIC <- -2*l + 2*K
  return(list(l = l, K = K, AIC = AIC))
  
}