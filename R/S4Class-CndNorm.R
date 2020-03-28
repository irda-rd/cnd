#' @title CND Norm (S4 Class)
#' @description S4 class that contains location and scatter estimates.
#' @slot location a \code{numeric}, location estimate.
#' @slot scatter a \code{matrix}, variance or covariance matrix estimate.
#' @export
#' @import methods
#' @name S4Class-CndNorm
#' @aliases CndNorm class:CndNorm CndNorm-class
#' @examples
#' #Examine slots for the class
#' getSlots("CndNorm")
#'
#' #Define the location and scatter
#' location <- 1:3
#' scatter <- matrix(1:9,3,3)
#'
#' #Generate an object of class CndNorm
#' norm <- CndNorm(location = location, scatter = scatter)
#'
setClass("CndNorm", slots = c(location = "numeric", scatter = "matrix"), prototype = list(location = numeric(0), scatter = matrix(numeric(0), 0, 0)))
#--------------------------------------------------------------------
#' @details The validity method check if: \code{location} is of class \code{numeric}; \code{scatter} is a numerical matrix; the location length correspond to the number of row and column of scatter.
#' @rdname S4Class-CndNorm
#' @name setValidity_CndNorm
setValidity(Class = "CndNorm", method <- function(object){
  #Initialize error msg
  errors <- character(0)

  #Ensure location is a numeric
  if(!is(object@location, "numeric")){
    msg <- "location is not a numeric"
    errors <- c(errors, msg)
  }

  #Ensure scatter is a numerical matrix
  if(!is(object@scatter, "matrix")){
    msg <- "scatter is not a numerical matrix"
    errors <- c(errors, msg)
  }else if(!is(as.vector(object@scatter), "numeric")){
    msg <- "scatter is not a numerical matrix"
    errors <- c(errors, msg)
  }

  #Ensure dimensionality
  if(!(nrow(object@scatter) == length(object@location) & ncol(object@scatter) == length(object@location))){
    msg <- "location length differs to the scatter's number of row or column"
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
#' @details The function \code{CndNorm} can be used to build an object.
#' @rdname S4Class-CndNorm
#' @export
CndNorm = function(location = numeric(0), scatter = matrix(numeric(0), 0, 0)){
  #Generate the object of class CndData and check
  cndNorm <- new("CndNorm", location = location, scatter = scatter)
  return(cndNorm)
}
