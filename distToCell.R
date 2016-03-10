#caculates distance to an observation from each cell of a raster using a cost matrix
#author: Lisa-Marie Harrison
#date: 11/03/2016

library(gdistance)

distToCell <- function(goal_coordinates, observation_coordinates, transition_layer) {
  
  #goal_coordinates: matrix of goal coordinates (lat, long) for final raster cells. Cells over island ommited
  #observation coordinates: matrix of all observation coordinates (lat, long)
  #transition_layer: Transition layer object for cost of transition between cells
  
  calcDist <- function(x) {
    
    dist_mat <- rep(NA, nrow(observation_coordinates))
    
    dist_guess <- gcdHF(deg2rad(x[2]), deg2rad(x[1]), deg2rad(observation_coordinates[, 1]), deg2rad(observation_coordinates[, 2]))
    
    #only check observations where guess distance is close
    if (any(dist_guess < 6)) {
      
      for (i in which(dist_guess < 6)) {
        
        tryCatch({
          shortest_path <- shortestPath(transition_layer, cbind(x[1], x[2]), 
                                        as.matrix(rev(observation_coordinates[i, ]), ncol = 2), "SpatialLines")
          dist_mat[i] <- SpatialLinesLengths(shortest_path)
        }, error = function(e) {
          print("ERROR: Looks like we got an error, skipping point")
        })
        
      }
    } 
    return (dist_mat)
  }
  
  dist_mat <- t(apply(goal_coordinates, 1, calcDist))
  
  return (dist_mat)
}