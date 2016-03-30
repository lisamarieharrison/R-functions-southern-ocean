#variation on dsm.var.gam from dsm package for variance calculation in dsm objects
#modified to pass NA values

gamVar <- function (dsm.obj, pred.data, off.set = NULL, seglen.varname = "Effort", 
          type.pred = "response") 
{
  class(dsm.obj) <- class(dsm.obj)[class(dsm.obj) != "dsm"]
  if (any(class(dsm.obj) == "gamm")) {
    dsm.obj <- dsm.obj$gam
    is.gamm <- TRUE
  }
  if (length(off.set) == 1) {
    if (is.null(nrow(pred.data))) {
      off.set <- rep(list(off.set), length(pred.data))
    }
    else {
      off.set <- rep(off.set, nrow(pred.data))
    }
  }
  if (is.data.frame(pred.data) & is.vector(off.set)) {
    pred.data <- list(pred.data)
    off.set <- list(off.set)
  }
  else if (is.list(off.set)) {
    if (length(pred.data) != length(off.set)) {
      stop("pred.data and off.set don't have the same number of elements")
    }
  }
  if (type.pred == "response") {
    tmfn <- dsm.obj$family$linkinv
    dtmfn <- function(eta) {
      
      der <- NULL
      for (i in 1:length(eta)) {
        if (!is.na(eta[i])) {
          der[i] <- numderiv(tmfn, eta[i])
        } else {
          der[i] <- NA
        }
        
      }
      return (der)
      
    }
  }
  else if (type.pred == "link") {
    tmfn <- identity
    dtmfn <- function(eta) {
      1
    }
  }
  cft <- coef(dsm.obj)
  preddo <- list(length(pred.data))
  dpred.db <- matrix(0, length(pred.data), length(cft))
  for (ipg in seq_along(pred.data)) {
    pred.data[[ipg]]$off.set <- rep(0, nrow(pred.data[[ipg]]))
    lpmat <- predict(dsm.obj, newdata = pred.data[[ipg]], 
                     type = "lpmatrix")
        lppred <- lpmat %**% cft
    if (length(off.set[[ipg]]) == 1) {
      this.off.set <- rep(off.set[[ipg]], nrow(pred.data[[ipg]]))
    }
    else {
      this.off.set <- off.set[[ipg]]
    }
    preddo[[ipg]] <- this.off.set %**% tmfn(lppred)
    dpred.db[ipg, ] <- colSums(dtmfn(lppred) * lpmat, na.rm = TRUE)
  }
  vpred <- dpred.db %**% tcrossprod(vcov(dsm.obj), dpred.db)
  if (is.matrix(vpred)) {
    vpred <- diag(vpred)
  }
  result <- list(pred.var = vpred, bootstrap = FALSE, pred = preddo, 
                 var.prop = FALSE, pred.data = pred.data, off.set = off.set, 
                 dsm.object = dsm.obj, seglen.varname = seglen.varname, 
                 type.pred = type.pred)
  class(result) <- "dsm.var"
  return(result)
}