<<<<<<< HEAD
#function from http://distancesampling.org/R/vignettes/mexico-analysis.html to plot predicted abundances on a polygon

grid_plot_obj <- function (fill, name, sp) {
  
  # given the argument fill (the covariate vector to use as the fill) and a name,
  # return a geom_polygon object
  # fill must be in the same order as the polygon data
  
  
=======
#code from http://distancesampling.org/R/vignettes/mexico-analysis.html#fn3 for plotting polygon grid with fill

grid_plot_obj <- function(fill, name, sp){
>>>>>>> f3a608fc0b716f14940b5b8d0bd8b42234da464c
  
  # what was the data supplied?
  names(fill) <- NULL
  row.names(fill) <- NULL
  data <- data.frame(fill)
  names(data) <- name
  
<<<<<<< HEAD
  spdf <- SpatialPolygonsDataFrame(sp, data)
=======
  spdf <- sp
  spdf@data <- data
  
>>>>>>> f3a608fc0b716f14940b5b8d0bd8b42234da464c
  spdf@data$id <- rownames(spdf@data)
  spdf.points <- fortify(spdf, region="id")
  spdf.df <- join(spdf.points, spdf@data, by="id")
  
  # seems to store the x/y even when projected as labelled as
  # "long" and "lat"
  spdf.df$x <- spdf.df$long
  spdf.df$y <- spdf.df$lat
  
  geom_polygon(aes_string(x="x",y="y",fill=name, group="group"), data=spdf.df)
<<<<<<< HEAD
  
=======
>>>>>>> f3a608fc0b716f14940b5b8d0bd8b42234da464c
}