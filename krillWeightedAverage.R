#calculates the weighted mean of krill around a sighting within a specified distance interval
#author: Lisa-Marie Harrison
#date: 11/03/2016

krillWeightedAverage <- function(distance, threshold, time) {
  
  #distance: matrix of distance to each krill bin from each sighting from distFromKrill function
  #threshold: the threshold distance in km from each sighting of krill bins to use
  #time: time_difference matrix of time (hrs) between all sightings and krill bins
  
  krill_mean <- NULL
  for (i in 1:ncol(distance)) {
    weight <- 1/distance[, i]
    weight[weight < 1/threshold] <- 0
    weight[time[, i] > 1] <- 0 #exclude krill measured >1 hr since sighting
    krill_mean[i] <- weighted.mean(x = krill$arealDen, w = weight, na.rm = TRUE) 
  }
  
  return(krill_mean)
  
}