#' @title CND Call (S4 Class)
#' @description S4 class that contains the function name and arguments to be use in a method, see \code{CndMethod}.
#' @slot fun the name of the function to call.
#' @slot args a \code{list} of argument to pass to the function. Cnd objects used as parameter in \code{fun}, such as \code{cndData} and \code{cndNorm} should not be part of \code{args}.
#' @name S4Class-CndCall
#' @aliases CndCall class:CndCall CndCall-class
#' @import methods
#' @export
#' @examples
#' #Examine slots for the class
#' getSlots("CndCall")
#'
#' #General example (illustrating the call for a transformation)
#' transformation <- CndCall(fun = "cndClr", args = list(dropNutrient = "x3"))
#'
setClass("CndCall", slots = c(fun = "character", args = "list"))
#---------------------------------------------
#' @details The function \code{CndCall} should be used to build an object.
#' @export
#' @rdname S4Class-CndCall
CndCall = function(fun = character(0), args = list()){
  #Generate the object of class CndMethod
  method <- new("CndCall", fun = fun, args = args)
  return(method)
}
