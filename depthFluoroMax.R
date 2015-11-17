#find depth of fluorescence maximum

depthFluoroMax <- function(dat) {
  
  #dat = data frame with columns stn and l.fluoro
  #returns a vector of length unique(dat$stn) with depth of fluoro maximum (m) at each station
  
  max.depth <- 0
  
  for (i in 1:length(unique(dat$stn))) {
    max.depth[i] <- which.max(dat$l.fluoro[dat$stn == unique(dat$stn)[i]])
  }
  
  return(max.depth)
  
}

