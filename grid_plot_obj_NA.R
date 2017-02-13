#same as grid_plot_obj but doesn't plot cells where data values are NA
#code modified from dsm package
#author: Lisa-Marie Harrison
#date: 18/11/2016

grid_plot_obj_NA <- function(fill, name, sp){
  
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
  
  spdf.df <- na.omit(spdf.df)
  
  
  geom_polygon(aes_string(x="x",y="y",fill=name, group="group"), data=spdf.df)
  
}