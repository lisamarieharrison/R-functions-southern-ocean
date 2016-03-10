#interpolates observations with lat/long coordinates to a raster with barriers specified in the dist_mat argument
#author: Lisa-Marie Harrison
#date: 11/03/2016


interpolateWithBarriers <- function(dist_mat, goal_coordinates, reference_grid, FUN, dat = NULL) {
  
  #dist_mat: matrix of distance of each goal coordinate to each observation from distToCell function
  #goal_coordinates: matrix of goal coordinates (lat, long) for final raster cells. Cells over island ommited
  #reference_grid: raster of shape and resolution for final raster
  #FUN: string giving function to aggregate observations by. Choice of "count", "sum", "mean"
  #dat: if FUN is "sum" or "mean", vector of observation values
  
  int_raster <- reference_grid
  int_raster <- setValues(int_raster, rep(NA, nrow(coordinates(int_raster))))
  
  x <- coordinates(reference_grid)[, 1]
  y <- coordinates(reference_grid)[, 2]
  
  interp <- NULL
  for (cell in 1:nrow(goal_coordinates)) {
    
    w <- which(x == goal_coordinates[cell, 1] & y == goal_coordinates[cell, 2])
    
    include <- which(na.omit(dist_mat[cell, ] <= 5))
    
    if (FUN == "count") {
      interp[w] <- length(include)
    }
    
    if (FUN == "mean") {
      interp[w] <- mean(dat[include])
    }
    
    if (FUN == "sum") {
      interp[w] <- sum(dat[include])
    }
    
  }
  
  int_raster <- setValues(int_raster, interp)
  
}
