#code from http://distancesampling.org/R/vignettes/mexico-analysis.html#fn3 for plotting polygon grid with fill

grid_plot_obj <- function(fill, name, sp){

  # what was the data supplied?
  names(fill) <- NULL
  row.names(fill) <- NULL
  data <- data.frame(fill)
  names(data) <- name

  spdf <- SpatialPolygonsDataFrame(sp, data)
  spdf <- sp
  spdf@data <- data
  
  spdf@data$id <- rownames(spdf@data)
  spdf.points <- fortify(spdf, region="id")
  spdf.df <- join(spdf.points, spdf@data, by="id")
  
  # seems to store the x/y even when projected as labelled as
  # "long" and "lat"
  spdf.df$x <- spdf.df$long
  spdf.df$y <- spdf.df$lat
  
  geom_polygon(aes_string(x="x",y="y",fill=name, group="group"), data=spdf.df)

}