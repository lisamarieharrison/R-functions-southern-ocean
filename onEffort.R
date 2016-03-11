#for every row in a data frame, check whether it is within any on effort times
#uses withinTimes.R and apply function
#author: Lisa-Marie Harrison
#date: 11/03/2016


onEffort <- function(data, start, end) {
  
  #if FALSE, remove the row, else move on to next row
  #data: data frame with each row an observation
  #start: vector of start date and times as a chron object
  #end: vector of end date and times as a chron object
  
  data_oneffort <- data[apply(data, 1, FUN = withinTimes, start = start_datetime, end = end_datetime), ]
  
  return (data_oneffort)
  
}