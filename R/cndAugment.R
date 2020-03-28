#' @title Augment data
#' @description Join additional data to a \code{CndData} object under new slots. Information as data.frame, with the same number of row as slots of \code{CndData} can be added under the slot \code{other}, while supplementary data with a different structure must be added in the \code{suppl}.
#' @param cndData an object of class \code{CndData} or \code{CndDataAugmented}.
#' @param other a \code{data.frame} of additional values to associate with points of \code{cndData}, it must possess the same number of lines than \code{yield} and \code{X}.
#' @param suppl \code{list} of supplementary information to pass along the object.
#' @return Return an object of class \code{CndDataAugmented} with the entries \code{other} and \code{suppl} added to the corresponding slots.
#' @details If \code{cndData} is already an object of class \code{CndDataAugmented}, the entry \code{other} is binded as a column to the existing slot's \code{data.frame}, while the entry \code{suppl} is combined to the corresponding slot \code{list}.
#' @export
#' @examples
#' #General example
#' ##Generate an object of class CndData
#' n <- 20
#' yield <- rnorm(n)
#' X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
#' label <- data.frame(label1 = LETTERS[1:n], label2 = 1:n)
#' data <- CndData(yield = yield, X = X, label = label)
#'
#' ##Augment data of class CndData
#' dataAugmented <- cndAugment(data, other = X, suppl = list(comment = "comment"))
#' dataAugmented
#'
#' ##Augment data of class CndDataAugmented
#' cndAugment(dataAugmented, other = X, suppl = list(comment2 = "comment2"))
#'
setGeneric("cndAugment", function(cndData, other = data.frame(), suppl = list()) standardGeneric("cndAugment"))
setMethod("cndAugment", signature(cndData = "CndData"), function(cndData, other = data.frame(), suppl = list()) {
  cndData <- CndDataAugmented(yield = cndData@yield, X = cndData@X, label = cndData@label)
  cndDataAugmented <- cndAugment(cndData, other = other, suppl = suppl)
  return(cndDataAugmented)
})
setMethod("cndAugment", signature(cndData = "CndDataAugmented"), function(cndData, other = data.frame(), suppl = list()) {
  #Generate other as a data.frame if not precised
  if(is(other,"data.frame") & all(dim(other) == c(0,0))){
    M <- matrix(1, ncol = 0, nrow = nrow(cndData@X))
    other <- as.data.frame(M)
  }else if(!is(other, "data.frame")){
    stop("the argument other must be a data.frame")
  }

  #Bind the new values to the older values
  cndData@other <- bind_cols(cndData@other, other)
  cndData@suppl <- c(cndData@suppl, suppl)
  return(cndData)
})
