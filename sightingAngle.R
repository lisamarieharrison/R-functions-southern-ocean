#calculate the absolute heading of a sighting taking into account ships heading
#author: Lisa-Marie Harrison
#date: 11/03/2016

sightingAngle <- function(x, gps) {
  
  #x = row of sighting
  #gps = full gps matrix
  
  angle <- as.numeric(x[which(names(x) == "Angle")])
  index <- as.numeric(x[which(names(x) == "GpsIndex")])
  
  if (length(gps$Heading[gps$Index == index]) > 0) {
    
    #lhs
    if (angle <= 90) {
      angle_true <- angle + gps$Heading[gps$Index == index]
    } else {
      angle_true <- gps$Heading[gps$Index == index] - (360 - angle)
    }
    
    if(angle_true < 0) {
      angle_true <- 360 + angle_true
    }
    
  } else {
    return (NA)
  }
  
  return (angle_true)
  
}