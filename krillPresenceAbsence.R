#creates krill presence/absence given a vector of krill density. Krill density = 0 is absence and > 0 is presence
#author: Lisa-Marie Harrison
#date: 24/11/2015

krillPresenceAbsence <- function(p) {
  
  #p = vector of krill density
  
  pa <- rep(NA, length(p))
  pa[p > 0] <- 1
  pa[p == 0] <- 0
  return(pa)
  
}