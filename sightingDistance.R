#find true lat and long of sighting using reticle
#average human eye height on monkey island height is 15.08m
#author: Lisa-Marie Harrison
#date: 11/03/2016

sightingDistance <- function(x, reticle) {
  
  #x: row of sighting matrix for apply function
  #reticle: reticle distance lookup table
  
  measured_reticle <- as.numeric(x[which(names(x) == "Reticles")])
  
  if (is.na(measured_reticle)) {
    
    hypotenuse <- as.numeric(x[which(names(x) == "Est.Distance")])/1000
    
  } else {
    
    hypotenuse <- reticle$km[which.min(abs(reticle$angle - measured_reticle))]
    
  }
  
  distance <- sqrt(hypotenuse^2 - 0.01508^2)
  
  return(distance)
  
}
