#' @title Subset CndData
#' @description Subset the slots \code{yield}, \code{X} and \code{label} of a \code{CndData} object, as well as the slot \code{other} for a \code{CndDataAugmented} object.
#' @param cndData an object of class \code{CndData} or \code{CndDataAugmented}.
#' @param subset \code{logical} indicating which row to keep.
#' @export
#' @examples
#' #General example
#' ##Generate an object of class CndData and CndDataAugmented
#' n <- 20
#' yield <- rnorm(n)
#' X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
#' data <- CndData(yield = yield, X = X)
#' dataAugmented <- cndAugment(data, other = X, suppl = list(comment = "comment"))
#'
#' ##Subset both data sets
#' subset <- sample(c(TRUE,FALSE), n, replace = TRUE)
#' cndSubsetData(data, subset)
#' cndSubsetData(dataAugmented, subset)
#'
setGeneric("cndSubsetData", function(cndData, subset) standardGeneric("cndSubsetData"))
setMethod("cndSubsetData", signature(cndData = "CndDataAugmented", subset = "logical"), function(cndData, subset) {
  #Checking for subset to be not na
  if(any(is.na(subset))){
    stop("subset must not be NA")
  }

  #Apply subsetting on the slot other
  other <- cndData@other[subset, , drop = FALSE]

  #Dispatch cndSubsetData on the pure CndData part
  pureCndData <- as(cndData, "CndData")
  pureCndData <- cndSubsetData(pureCndData, subset)

  #Joining pure and augmented part
  cndData <- cndAugment(pureCndData, other = other)

  return(cndData)
})
setMethod("cndSubsetData", signature(cndData = "CndData", subset = "logical"), function(cndData, subset) {
  #Checking for subset to be not na
  if(any(is.na(subset))){
    stop("subset must not be NA")
  }

  #Apply subsetting on each slot of CndData
  ##Yield and X
  cndData@yield <- cndData@yield[subset, , drop = FALSE]
  cndData@X <- cndData@X[subset, , drop = FALSE]
  cndData@label <- cndData@label[subset, , drop = FALSE]
  return(cndData)
})

