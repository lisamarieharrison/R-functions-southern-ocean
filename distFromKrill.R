#returns distance in kms of each krill bin from each sighting
#author: Lisa-Marie Harrison
#date: 11/03/2016

distFromKrill <- function (x, krill, gps, truePosition=FALSE) {
  
  #x: row of sighting from apply function
  #krill: full matrix of krill locations
  #gps: gps lookup matrix for whale sightings
  #truePosition: is the x matrix used the true position of the sighting or the observation location (default)
  #set truePosition=TRUE if using true_lat_long with colnames "Latitude" and "Longitude"
  
  distance <- NULL
  
  if(!truePosition) {
    
    origin_long <- deg2rad(gps$Longitude[gps$Index == as.numeric(x[which(names(x) == "GpsIndex")])])
    origin_lat  <- deg2rad(gps$Latitude[gps$Index == as.numeric(x[which(names(x) == "GpsIndex")])])
    
  } else {
    
    origin_long <- deg2rad(as.numeric(x[which(names(x) == "Longitude")]))
    origin_lat  <- deg2rad(as.numeric(x[which(names(x) == "Latitude")]))
    
  }
  
  for (j in 1:length(krill$Latitude)) {
    distance[j] <- gcdHF(origin_lat, origin_long, deg2rad(krill$Latitude)[j], deg2rad(krill$Longitude)[j])    
  }
  
  distance[distance > 10000] <- NA
  
  return(distance)
  
}