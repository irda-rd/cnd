#' @title Estimate the reference
#' @description Generate a reference object, applying the \code{subset}, \code{norm} and \code{distance} components of the method, that can be used for analysis on other data sets.
#' @param transfData an object of class \code{CndData}, with \code{X} already transformed.
#' @param cndMethod an object of class \code{CndMethod}.
#' @details First only preserve a \code{subset} of the data, then calculate the \code{norm} and compute the \code{distance} for this subset. Each step is specified by \code{cndMethod}.
#' @return Return a \code{reference} object that contains the subset (a \code{CndDataAugmented} object that is also containing the distance) and the norm (of class \code{CndNorm}).
#' @export
#' @examples
#' #General example
#' ##Generate an object of class CndData
#' n <- 50
#' yield <- 100 + rnorm(n)
#' X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
#' cndData <- CndData(yield = yield, X = X)
#'
#' ##Identify the method and transform the data
#' cndMethod <- cndMethodLandry(dropNutrient = "x3")
#' transfData <- cndTransform(cndData, cndMethod)
#'
#' ##Compute the reference according to the defined method
#' cndReference(transfData, cndMethod)
#'
setGeneric("cndReference", function(transfData, cndMethod) standardGeneric("cndReference"))
setMethod("cndReference", signature(transfData = "CndData", cndMethod = "CndMethod"), function(transfData, cndMethod) {
  #Identify the (first) subset of the transformed data
  subsetData <- do.call(cndMethod@subset@fun, args = c(cndData = transfData, cndMethod@subset@args))

  #Comput norm and distance on the subsetted data
  norm       <- do.call(cndMethod@norm@fun, args = c(cndData = subsetData, cndMethod@norm@args))
  subsetData <- do.call(cndMethod@distance@fun, args = c(cndData = subsetData, cndNorm = norm, cndMethod@distance@args))

  #Reference object assemblage
  cndReference <- CndReference(subset = subsetData, norm =  norm)

  return(cndReference)
})
