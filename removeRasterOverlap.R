#remove cells of a raster that overlap a polygon by setting to NA
#author: Lisa-Marie Harrison
#date: 11/03/2016


removeRasterOverlap <- function(rast, poly, allowance) {
  
  #rast: raster object
  #poly: polygon of boundaries to remove from raster (e.g. island)
  #allowance: percentage of cell overlap allowed. Eg 20 = 20% of a raster cell can be overlapped by polygon
  
  poly_overlap <- getValues(rasterize(poly, rast, getCover=TRUE))
  remove_overlap <- getValues(rast)
  remove_overlap[poly_overlap >= allowance] <- NA
  rast <- setValues(rast, remove_overlap)
  return (rast)
  
}