#' @title Bind CndData
#' @description Bind slots of a \code{CndData}, or a \code{CndDataAugmented}, object into a single \code{data.frame}. For the latter, only the slot \code{other} is combined to the slots \code{X}, \code{Y} and \code{label}; the slot \code{suppl} is not used.
#' @param cndData An object of class \code{CndData} or \code{CndDataAugmented}.
#' @return Return a single \code{data.frame}.
#' @export
#' @import dplyr
#' @examples
#' #' #General example
#' ##Generate objects of classes CndData and CndDataAugmented
#' n = 20
#' yield <- rnorm(n)
#' X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
#' data <- CndData(yield = yield, X = X)
#' dataAugmented <- cndAugment(data, other = X, suppl = list(comment = "comment"))
#'
#' ##Bind elements into a data.frame for each class
#' cndBind(data)
#' cndBind(dataAugmented)
#'
setGeneric("cndBind", function(cndData) standardGeneric("cndBind"))
setMethod("cndBind", signature(cndData = "CndDataAugmented"), function(cndData) {

  if(all(dim(cndData@label) == c(0,0))){
    label <- NULL
  }else{
    label <- cndData@label
  }
   df <- bind_cols(label, cndData@yield, cndData@X, cndData@other)
  return(df)
})
setMethod("cndBind", signature(cndData = "CndData"), function(cndData) {
  if(all(dim(cndData@label) == c(0,0))){
    label <- NULL
  }else{
    label <- cndData@label
  }
  df <- bind_cols(label, cndData@yield, cndData@X)
  return(df)
})

