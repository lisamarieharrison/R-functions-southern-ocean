#check.cols function from dsm that wasn't loading properly so need to source


check.cols <- function (ddf.obj, segment.data, observation.data, strip.width, 
          segment.area) 
{
  checks <- list(segment.data = c("Effort", "Sample.Label"), 
                 observation.data = c("object", "Sample.Label", "size", 
                                      "distance"))
  for (i in 1:length(checks)) {
    check.res <- checks[[i]] %in% names(get(names(checks)[[i]]))
    if (any(!check.res)) {
      stop(paste0("Column(s) \"", paste(checks[[i]][!check.res], 
                                        collapse = "\", \""), "\" not found in ", names(checks)[[i]], 
                  ".\n  Check ?\"dsm-data\"."))
    }
  }
  if (length(segment.data$Sample.Label) != length(unique(segment.data$Sample.Label))) {
    warning("'Sample.Labels are non-unique in segment data!")
  }
  invisible()
}