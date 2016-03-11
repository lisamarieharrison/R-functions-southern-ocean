#calculates the number of individual whales within a specified distance of each krill bin
#for use with apply function
#author: Lisa-Marie Harrison
#date: 11/03/2016


countIndividualsAroundKrill <- function (x, sighting, threshold) {
  
  #x: row of distance matrix for apply function
  #sighting: full sighting matrix
  #threshold: maximum distance (km) for a sighting to be included
  
  count <- sum(sighting$BestNumber[which(x < threshold)])
  
  return (count)
  
}