#' @title Transformations from the package compositions
#' @description Wrapper of transformations \code{clr}, \code{ilr} and \code{alr} from the package compositions, to take \code{CndData} object as entry.
#' @param cndData an object of class \code{CndData}.
#' @param dropNutrient \code{character}, the name of the nutrient (column name of the slot \code{X} of \code{CndData}) to drop after the transformation.
#' @param ... other parameters to pass to the original function of the package \code{compositions}.
#' @return All functions return a transformed \code{CndData} object.
#' @details Only the requiered functions are called when needed, using the double colon symbol, to avoid namespace conflicts. The argument \code{dropNutrient} in \code{cndClr} has been added from the original function \code{clr}.
#' @name cndCompositions
#' @examples
#' #General data
#' ##CndData
#' n <- 20
#' yield <- 100 + rnorm(n)
#' X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
#' label <- LETTERS[(seq_len(n)-1)%%4+1]
#' cndData <- CndData(yield = yield, X = X, label = label)
#'
#' #Transform the compositional component of the data,
#' cndIlr(cndData)
#' cndClr(cndData, dropNutrient = "x3")
#' cndAlr(cndData)
NULL
#' @rdname cndCompositions
#' @export
setGeneric("cndClr", function(cndData, dropNutrient = NULL, ...) standardGeneric("cndClr"))
setMethod("cndClr", signature(cndData = "CndData"), function(cndData, dropNutrient = NULL, ...) {
  cndData@X <- as.data.frame(compositions::clr(x = as.matrix(cndData@X), ...))
  cndData@X[[dropNutrient]] <- NULL
  return(cndData)
})

#' @rdname cndCompositions
#' @export
setGeneric("cndIlr", function(cndData, ...) standardGeneric("cndIlr"))
setMethod("cndIlr", signature(cndData = "CndData"), function(cndData, ...) {
  cndData@X <- as.data.frame(compositions::ilr(x = as.matrix(cndData@X), ...))
  return(cndData)
})

#' @rdname cndCompositions
#' @export
setGeneric("cndAlr", function(cndData, ...) standardGeneric("cndAlr"))
setMethod("cndAlr", signature(cndData = "CndData"), function(cndData, ...) {
  cndData@X <- as.data.frame(compositions::alr(x = as.matrix(cndData@X), ...))
  return(cndData)
})
