#function to calculate weighted average of variable around a specified location within a distance threshold
#based on krillToGrid
#author: Lisa-Marie Harrison
#date: 06/04/2016

envToGrid <- function (x, threshold, data_frame, variable) {
  
  #x: coordinates in utm (easting, northing) for a grid location to calculate variable mean around
  #threshold: the threshold distance in km from each sighting of variable bins to use
  #data_frame: matrix containing x (easting), y (northing) and variable
  #variable: 
  
  var_distances <- sqrt((x[1] - data_frame$x)^2 + (x[2] - data_frame$y)^2)/1000
  
  weight <- 1/var_distances
  weight[weight < 1/threshold] <- 0
  var_mean <- weighted.mean(x = data_frame[, variable], w = weight, na.rm = TRUE) 
  
  if (is.nan(var_mean)) {
    
    var_mean <- NA
    
  }
  
  return(var_mean)
  
}
