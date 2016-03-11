#calculates the true latitude and longitude of an object from its bearing, distance and point of observation
#author: Lisa-Marie Harrison
#date: 11/03/2016


sightingLatLong <- function (x, gps) {
  
  #x = row of sighting
  #gps = full gps matrix
  #distance = distance to object in m
  
  index <- as.numeric(x[which(names(x) == "GpsIndex")])
  angle <- as.numeric(x[which(names(x) == "angle_true")])
  distance <- as.numeric(x[which(names(x) == "distance")])*1000
  
  if (length(gps$Longitude[gps$Index == index]) > 0) {
    
    lon <- destPoint(p = c(gps$Longitude[gps$Index == index], gps$Latitude[gps$Index == index]),
                     b = angle, d = distance)[1]
    
    lat <- destPoint(p = c(gps$Longitude[gps$Index == index], gps$Latitude[gps$Index == index]),
                     b = angle, d = distance)[2]
  } else {
    
    lon <- NA
    lat <- NA
    
  }
  
  true_lat_long <- cbind(lat, lon)
  names(true_lat_long) <- c("Latitude", "Longitude")
  
  return(true_lat_long)
  
}