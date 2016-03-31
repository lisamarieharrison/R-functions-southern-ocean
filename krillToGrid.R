#function to calculate weighted average krill density (gm2) around a specified location within a threshold
#author: Lisa-Marie Harrison
#date: 21/03/2016

krillToGrid <- function (x, threshold, krill_mat) {
  
  #x: coordinates in utm (easting, northing) for a grid location to calculate krill around
  #threshold: the threshold distance in km from each sighting of krill bins to use
  #krill_mat: matrix containing x (easting), y (northing) and krill (areal density)
  
    krill_distances <- sqrt((x[1] - krill_mat$x)^2 + (x[2] - krill_mat$y)^2)/1000
  
    weight <- 1/krill_distances
    weight[weight < 1/threshold] <- 0
    krill_mean <- weighted.mean(x = krill_mat$krill, w = weight, na.rm = TRUE) 

    if (is.nan(krill_mean)) {
      
      krill_mean <- NA
      
    }
    
  return(krill_mean)
  
}