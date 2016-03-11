#calculates time in hours between each sighting and each krill bin
#author: Lisa-Marie Harrison
#date: 11/03/2016


krillBinTimeDiff <- function (sighting, krill) {
  
  #sighting: full sighting matrix with datetime column
  #krill: full krill matrix with datetime column
  
  diff_hours <- matrix(NA, nrow = nrow(krill), ncol = nrow(sighting))
  
  for (i in 1:length(sighting$datetime)) {
    for (j in 1:length(krill$datetime)) {
      diff_hours[j, i] <- abs(as.numeric(sighting$datetime[i] -  krill$datetime[j])*24)
    }
  }
  
  return(diff_hours)
  
}
