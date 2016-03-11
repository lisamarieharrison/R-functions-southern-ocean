#check if a row is between any on effort times
#for use with apply function in onEffort.R
#author: Lisa-Marie Harrison
#date: 11/03/2016

library(chron)

withinTimes <- function(x, start, end) {
  
  #dataRow: row of a data frame of observations with datetime column
  #start: vector of start date and times as a chron object
  #end: vector of end date and times as a chron object
  
  datetime_chron <- x[which(names(x) == "datetime")]
  
  datetime <- chron(dates. = substr(datetime_chron, 2, 9), times. = substr(datetime_chron, 11, 18), 
                    format = c(dates = "d/m/y", times = "h:m:s"))
  
  return (any(start < datetime & end > datetime)) 
  
}