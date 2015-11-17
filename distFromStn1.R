#distance of each station from station 1 in x and y directions

source("C:/Users/Lisa/Documents/phd/southern ocean/Mixed models/R code/functions_southern_ocean/gcdHF.R")
source("C:/Users/Lisa/Documents/phd/southern ocean/Mixed models/R code/functions_southern_ocean/deg2rad.R")

distFromStn1 <- function(lat, long) {
  
  #returns distance in 100 kms
  
  x <- 0
  y <- 0
  rad_long <- deg2rad(long)
  rad_lat  <- deg2rad(lat)
  top_lat <- deg2rad(max(lat))
  top_long <- deg2rad(max(long))
  for (i in 1:length(lat)) {
    x[i] <- gcdHF(rad_lat[i], top_long, top_lat, top_long)/100    
    y[i] <- gcdHF(top_lat, rad_long[i], top_lat, top_long)/100
  }
  
  return(list(x = x, y = y))
  
}


