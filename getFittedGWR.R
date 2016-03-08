#Function to calculate fitted values from a poisson geographically weighted regression model for any coefficient names
#author: Lisa-Marie Harrison
#date: 08/03/2016

getFittedGWR <- function(model, dat) {
  
  #model: a gwr model object from gwr.generalised 
  #dat: data.frame of data with same column names as coefficients in model
  
  coefs <- names(model$glm.res$coefficients)
  
  if (any(coefs == "Intercept")) {
    
    coefs <- coefs[!coefs == "Intercept"]
    
    est <- as.data.frame(model$SDF)[, match(coefs, names(model$SDF))]
    df <- dat[, na.omit(match(coefs, names(dat)))]
    
    model.fitted <- exp(rowSums(est * df) + model$SDF$Intercept)   
    
  } else {
    
    est <- as.data.frame(model$SDF)[, na.omit(match(names(model$SDF), coefs))]
    
    df <- dat[, match(coefs, names(dat))]
    model.fitted <- exp(rowSums(est * df))
    
  }
  
  return (model.fitted)
  
}
