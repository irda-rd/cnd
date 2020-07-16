#' @title Transform CndData
#' @description Perform the \code{transformation}, specified by \code{cndMethod}, on the \code{X} component of the \code{CndData} object.
#' @param cndData an object of class \code{CndData}.
#' @param cndMethod an object of class \code{CndMethod}.
#' @return Return a \code{CndData} object of the transformed data.
#' @export
#' @examples
#' #Generate random data for the example
#' n <- 50
#' yield <- 100 + rnorm(n)
#' X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
#' label <- LETTERS[(seq_len(n)-1)%%4+1] #Alternative for black and white: label <- NULL
#'
#' #Generate an object of class CndData.
#' cndData <- CndData(yield = yield, X = X, label = label)
#'
#' #Generate an object of class CndMethod.
#' cndMethod <- cndMethodRobustIrda(dropNutrient = "x3", labelName = "label")
#'
#' #Transform the composition (X) of cndData the using cndMethod
#' cndTransform(cndData, cndMethod)
#'
setGeneric("cndTransform", function(cndData, cndMethod) standardGeneric("cndTransform"))
setMethod("cndTransform", signature(cndData = "CndData", cndMethod = "CndMethod"), function(cndData, cndMethod) {
  cndData <- do.call(cndMethod@transformation@fun, args = c(cndData = cndData, cndMethod@transformation@args))
  return(cndData)
})
