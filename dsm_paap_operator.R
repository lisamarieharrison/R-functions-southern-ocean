#function from dsm package for local testing

'%**%' <- function (x, y) {

    dimnames(x) <- NULL
    dimnames(y) <- NULL
    if (length(dim(x)) == 2 && length(dim(y)) == 2 && dim(x)[2] == 
        1 && dim(y)[1] == 1) 
      return(c(x) %o% c(y))
    if ((!is.null(dim(x)) && any(dim(x) == 1))) 
      dim(x) <- NULL
    if ((!is.null(dim(y)) && any(dim(y) == 1))) 
      dim(y) <- NULL
    if (is.null(dim(x)) && is.null(dim(y))) {
      if (length(x) == length(y)) 
        x <- x %*% y
      else {
        if ((length(x) != 1) && (length(y) != 1)) 
          stop(paste("lengths of x (", length(x), ") and y (", 
                     length(y), ") are incompatible", sep = ""))
        else x <- x * y
      }
    }
    else x <- x %*% y
    if ((!is.null(dim(x)) && any(dim(x) == 1))) 
      dim(x) <- NULL
  
  x
}