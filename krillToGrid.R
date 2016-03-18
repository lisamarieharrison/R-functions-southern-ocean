#calculates the weighted mean of krill around a grid cell with a specified threshold
#author: Lisa-Marie Harrison
#date: 18/03/2016

krillToGrid <- function(x, threshold) {
  
  #x: row of coordinates (easting, northing)
  #threshold: the threshold distance (km) from each sighting of krill bins to use

  dists <- sqrt((x[1] - segdata$x)^2 + (x[2] - segdata$y)^2)/1000
  
  weight <- 1/dists
  weight[weight < 1/threshold] <- 0
  krill_mean <- weighted.mean(x = segdata$krill, w = weight, na.rm = TRUE) 
  
  if (is.nan(krill_mean)) {
    krill_mean <- NA
  }
  
  return(krill_mean)
  
}