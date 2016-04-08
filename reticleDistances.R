#calculate the distance to a sighting from the reticle angle and observer height for Balleny Island data
#author: Lisa-Marie Harrison
#date: 04/04/2016

reticleDistances <- function(x) {
  
  #x: row of reticle matrix for apply function

  hypotenuse <- as.numeric(x[which(names(x) == "km")])

  distance <- sqrt(hypotenuse^2 - 0.01508^2)
  
  return(distance)
  
}
