#' @title CND Minimum covariance determinant estimates (S4 Class)
#' @description S4 class that extend the class \code{CndNorm} to also include the logarithm of the covariance matrix determinant \code{logDet} associated to the robust method.
#' @slot location a \code{numeric}, location estimate.
#' @slot scatter a \code{matrix}, variance or covariance matrix estimate.
#' @slot logDet a \code{matrix}, logarithm of the determinant of the covariance matrix, which was minimized in the mcd.
#' @slot subset a \code{CndData} object, representing the subset of observation that minimized the determinant, and used to establish the location and scatter.
#' @include S4Class-CndNorm.R
#' @export
#' @import methods
#' @aliases McdNorm class:McdNorm McdNorm-class
#' @name S4Class-McdNorm
#' @examples
#' #Examine slots for the class
#' getSlots("McdNorm")
#'
#' #General example
#' ##Define elements provided in by the mcd
#' ###The norm and logDet
#' location <- 1:3
#' scatter <- matrix(1:9,3,3)
#' logDet <- 1
#'
#' ##A data subset (here some CndData object)
#' n <- 10
#' yield <- rnorm(n)
#' X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
#' label <- LETTERS[(seq_len(n)-1)%%4+1]
#' subset <- CndData(yield = yield, X = X, label = label)
#'
#' ##Generate an object of class McdNorm
#' norm <- McdNorm(location = location, scatter = scatter,logDet = logDet, subset = subset)
#'
setClass("McdNorm", slots = c(logDet = "numeric", subset = "CndData"), contains = "CndNorm")
#' @details The validity method check if: \code{location} and \code{scatter} respect validity checks defined for \code{CndNorm} and if \code{logDet} only possess one value.
#' @rdname S4Class-McdNorm
#' @name setValidity_McdNorm
#--------------------------------------------------------------------
setValidity(Class = "McdNorm", method <- function(object){
  #Initialize error msg
  errors <- character(0)

  #Ensure there is only one value for logDet
  if(length(object@logDet) > 1){
    msg <- "A single value is expected for logDet"
    errors <- c(errors, msg)
  }

  #Return TRUE if the object is valid, the error msg vector otherwise
  if(length(errors) == 0) {
    check = TRUE
  }else{
    check = errors
  }
  return(check)
})
#--------------------------------------------------------------------
#' @details The function \code{McdNorm} can be used to build an object.
#' @rdname S4Class-McdNorm
#' @export
McdNorm = function(location = numeric(0), scatter = matrix(numeric(0), ncol = 0, nrow = 0), logDet = numeric(0), subset = CndData()){
  #Generate the object of class RobustMSD. Allows empty object, but does not validate dimensions.
  object <- new("McdNorm", location = location, scatter = scatter, logDet = logDet, subset = subset)
  return(object)
}
#--------------------------------------------------------------------

