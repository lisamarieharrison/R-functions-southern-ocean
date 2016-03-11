#creates a data frame of explanatory variables, predictors and coordinates from rasters
#author: Lisa-Marie Harrison
#date: 11/03/2016

dfFromRaster <- function (variable_stack, centre_vars, returnLatLong=TRUE) {
  
  #variable_stack: raster stack of all variables including explanatory and predictors (not including coordinates)
  #scale_vars: vector of variable_stack indices to centre at 0
  #returnLatLong: Boolean indicating if lat/long coordinates should be extracted from the raster and returned
  #note: latitude and longitude are not centred
  
  #dist from top in km
  
  if (returnLatLong) {
    
    x <- coordinates(variable_stack)[, 1]
    y <- coordinates(variable_stack)[, 2]
    
    d <- data.frame(cbind(x, y, getValues(variable_stack)))
    d <- data.frame(cbind(d[, c(1, 2, which(!(1:nlayers(variable_stack) %in% centre_vars)) + 2)], apply(d[, centre_vars + 2], 2, FUN = scale, scale = FALSE)))
    names(d) <- c("long", "lat", names(variable_stack))
    
  } else {
    
    d <- data.frame(getValues(variable_stack))
    d <- data.frame(cbind(d[, c(which(!(1:nlayers(variable_stack) %in% centre_vars)))], apply(d[, centre_vars], 2, FUN = scale, scale = FALSE)))
    names(d) <- names(variable_stack)
    
  }
  
  
  d <- na.omit(d)
  
  return (d)
  
}

