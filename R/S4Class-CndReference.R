#' @title CND Reference (S4 Class)
#' @description S4 class that contains the subset selected prior to the norm calculation as well as the norm. It might be used to perform analysis on another dataset. The function \code{CndReference} should be used to build an object.
#' @slot subset a \code{CndData} object representing a selection of data prior to the norm calculation. Does not includes further selection while calculating the norm.
#' @slot norm a \code{CndCNorm} object associated to the subset.
#' @include S4Class-CndData.R
#' @include S4Class-CndNorm.R
#' @include cndAugment.R
#' @export
#' @import methods
#' @name S4Class-CndReference
#' @aliases CndReference class:CndReference CndReference-class
#' @examples
#' #Examine slots for the class
#' getSlots("CndReference")
#'
#' #General example
#' ##Generate an object of class CndData
#' n <- 50
#' yield <- 100 + rnorm(n)
#' X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
#' label <- LETTERS[(seq_len(n)-1)%%4+1]
#' cndData <- CndData(yield = yield, X = X, label = label)
#'
#' ##Generate an object of class CndNorm
#' ##Define the location and scatter
#' location <- 1:3
#' scatter <- matrix(1:9,3,3)
#' cndNorm <- CndNorm(location = location, scatter = scatter)
#'
#' ##Generate an object of class CndReference
#' reference <- CndReference(subset = cndData, norm = cndNorm)
#'
setClass("CndReference", slots = c(subset = "CndData", norm = "CndNorm"), prototype = list(subset = cndAugment(CndData()),norm = CndNorm()))
#---------------------------------------------
#' @title Generate an object of class \code{Inflection}
#' @description Create a class \code{CndMethod}. See \linkS4class{CndMethod} for examples.
#' @export
CndReference = function(subset = cndAugment(CndData()), norm =  CndNorm()){
  #Generate the object of class CndReference. Allows for empty object, but does not validate dimensions.
  object <- new("CndReference", subset = subset, norm = norm)
  return(object)
}
#---------------------------------------------


