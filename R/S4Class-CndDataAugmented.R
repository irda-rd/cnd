#' @title CND Data Augmented (S4 class)
#' @description S4 class that contains information on the data to use for the cnd analysis, but that also contains additional fields.
#' @slot yield \code{data.frame}, with one column, of yield associated to lines of \code{X}.
#' The entry can be a \code{numeric} vector if the object is built using the function \code{CndData}.
#' @slot X \code{data.frame} of composition, with columns corresponding to nutrients and lines to samples.
#' @slot label (optional) \code{data.frame} of labels associated to lines of \code{X}. By default, an empty \code{data.frame} with the same number of rows as \code{yield} and \code{X}, but no columns.
#' @slot other a \code{data.frame} of additional values to associate with points of \code{cndData}, it must possess the same number of lines than \code{yield} and \code{X}.
#' @slot suppl \code{list} of supplementary material to pass along the object.
#' @export
#' @import methods
#' @name S4Class-CndDataAugmented
#' @aliases CndCndDataAugmented class:CndDataAugmented CndDataAugmented-class
#' @include S4Class-CndData.R
#' @examples
#' #General example
#' #' ##Observe slots of CndMethod
#' getSlots("CndDataAugmented")
#'
#' ##Generate data for the example
#' n <- 20
#' yield <- rnorm(n)
#' X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
#' label <- LETTERS[(seq_len(n)-1)%%4+1]
#'
#' ##Generate an object of class CndDataAugmented
#' data <- CndDataAugmented(yield = yield, X = X, label = label,
#' other = X, suppl = list(comment = "some comment"))
#'
setClass("CndDataAugmented", slots = c(other = "data.frame", suppl = "list"), contains = "CndData")
#--------------------------------------------------------------------
#' @details The validity method check if: \code{yield} only possesses one column; \code{yield}, \code{X}, \code{label} and \code{other} have the same number of lines; columns of \code{yield} and \code{X} are of class \code{numeric}.
#' @rdname S4Class-CndDataAugmented
#' @name setValidity_CndDataAugmented
setValidity(Class = "CndDataAugmented", method <- function(object){
  #Initialize error msg
  errors <- character(0)

  #Ensure other possess the same number of row as X and yield
  if((nrow(object@other) != nrow(object@X)) | (nrow(object@other) != nrow(object@yield))){
    msg <- "Number of rows of other differs from those of X or yield"
    errors <- c(errors, msg)
  }

  #Return TRUE if the object is valid, the error msg vector otherwise
  if (length(errors) == 0) {
    check = TRUE
  }else{
    check = errors
  }
  return(check)
})
#--------------------------------------------------------------------
setMethod("initialize", "CndDataAugmented", function(.Object, other = data.frame(), suppl = list(), ...) {
    #If other is empty, make the number of row correspond to X
    args <- list(...)
    if(all(dim(other) == c(0,0)) & ("X" %in% names(args))){
    M <- matrix(1, ncol = 0, nrow = nrow(args$X))
    other <- as.data.frame(M)
  }

   #Assignation and call for the default initialization with the new values
  .Object@other <- other
  .Object@suppl <- suppl
  .Object <- callNextMethod(.Object, ...)

  return(.Object)
})
#--------------------------------------------------------------------
#' @details The function \code{CndDataAugmented} should be used to build an object. Both arguments \code{yield} and \code{label} can be provided as vectors, they will be coerced into \code{data.frame}. If provided as vector, the column name of \code{label} would be "label", and the one \code{yield} would be "yield". For the latter, the column name would also be overwriten as "yield" if the entry is a \code{data.frame}. If empty, the number of rows of \code{label} would be matched to those of \code{yield} and \code{X} at initialisation.
#' @rdname S4Class-CndDataAugmented
#' @export
#'
CndDataAugmented = function(yield = data.frame(yield = numeric(0)), X = data.frame(), label = data.frame(), other = data.frame(), suppl = list()){
  #Generate the object of class CndData and check
  data <- new("CndDataAugmented", yield = yield, X = X, label = label, other = other, suppl = suppl)
  return(data)
}
