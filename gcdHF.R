#Calculates the distance between two points with radian latitude/longitude using Haversine formula

gcdHF <- function(lat1, long1, lat2, long2) {
  
  #input latitudes and longitudes in radians
  #returns distance in km
  
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  
  return(d) # Distance in km
  
}