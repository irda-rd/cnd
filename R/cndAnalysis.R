#' @title Analyse a data set from a norm
#' @description Compute the distance on a new dataset, using a norm, and perform supplementary analysis.
#' @param transfData an object of class \code{CndData}, with \code{X} already transformed.
#' @param cndMethod an object of class \code{CndMethod}.
#' @param cndNorm an object of class \code{CndNorm}.
#' @return Return a \code{CndDatAugmented} object corresponding to \code{transfData} augmented to also include the distance and the results of the supplementary analysis.
#' @details The slots \code{distance} of \code{cndMethod} make use of the \code{cndNorm} to compute distance of each observation of \code{transfData} from the centroid. The result is joined to \code{transfData}, usually as a column named \code{distance} under the slot \code{other}. The new object is then used to perform other analysis, specified by the slot \code{analysis} of \code{cndMethod}, which also append the result to \code{transfData}, usually in the \code{suppl} slot.
#' @import catenelson
#' @export
#' @examples
#' #Generate an object of class CndData.
#' n <- 50
#' yield <- 100+rnorm(n)
#' X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
#' cndData <- CndData(yield = yield, X = X)
#'
#' #Compute a reference
#' cndMethod <- cndMethodRobustIrda(dropNutrient = "x3")
#' transfData <- cndTransform(cndData, cndMethod)
#' cndReference <- cndReference(transfData, cndMethod)
#'
#' #Perform the analysis
#' cndAnalysis(transfData, cndMethod, cndNorm = cndReference@norm)
#'
setGeneric("cndAnalysis", function(transfData, cndMethod, cndNorm) standardGeneric("cndAnalysis"))
setMethod("cndAnalysis", signature(transfData = "CndData", cndMethod = "CndMethod", cndNorm = "CndNorm"), function(transfData, cndMethod, cndNorm) {
  #Distance
  ##Computing distance a new data set, using the norm
  transfData <- do.call(cndMethod@distance@fun, args = c(cndData = transfData, cndNorm = cndNorm, cndMethod@distance@args))

  ##Further analysis implicating the data and/or the distance
  transfData <- do.call(cndMethod@analysis@fun, args = c(cndData = transfData, cndMethod@analysis@args))

  return(transfData)
})
