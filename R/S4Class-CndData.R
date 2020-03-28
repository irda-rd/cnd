#' @title CND Data (S4 class)
#' @description S4 class that contains information on the data to use for the cnd analysis.
#' @slot yield a \code{data.frame}, with one column, of yield associated to lines of \code{X}.
#' The entry can be a \code{numeric} vector if the object is built using the function \code{CndData}.
#' @slot X a \code{data.frame} of composition, with columns corresponding to nutrients and lines to samples.
#' @slot label (optional) \code{data.frame} of labels associated to lines of \code{X}. By default, an empty \code{data.frame} with the same number of rows as \code{yield} and \code{X}, but no columns.
#' @export
#' @import methods
#' @name S4Class-CndData
#' @aliases CndData class:CndData CndData-class
#' @examples
#' #Examine slots for the class
#' getSlots("CndData")
#'
#' #General example
#' ##Generate an object of class CndData
#' n <- 20
#' yield <- rnorm(n)
#' X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
#' label <- LETTERS[(seq_len(n)-1)%%4+1]
#'
#' ##Generate an object of class CndData.
#' data <- CndData(yield = yield, X = X, label = label)
#'
#' #Examples with zero or multiple label columns
#' ##Zero columns
#' data <- CndData(yield = yield, X = X)
#'
#' ##Two columns
#' label <- data.frame(label1 = label, label2 = 1:n)
#' data <- CndData(yield = yield, X = X, label = label)
#'
setClass("CndData", slots = c(yield = "data.frame", X = "data.frame", label = "data.frame"))
#--------------------------------------------------------------------
#' @details The validity method check if: \code{yield} only possesses one column; \code{yield}, \code{X} and \code{label} have the same number of lines; columns of \code{yield} and \code{X} are of class \code{numeric}.
#' @rdname S4Class-CndData
#' @name setValidity_CndData
setValidity(Class = "CndData", method <- function(object){
  #Initialize error msg
  errors <- character(0)

  #Ensure yield possess only one column
  if(ncol(object@yield) != 1){
    msg <- "Specified yield does not possesses only one column"
    errors <- c(errors, msg)
  }

  #Validate the proper length (number of rows) of slots
  ##Case of yield
  if(nrow(object@X) != nrow(object@yield)){
    msg <- "Specified X and yield differ in number of lines"
    errors <- c(errors, msg)
  }
  ##Case of label (if defined)
  if(nrow(object@X) != nrow(object@label)){
    msg <- "Specified X and labels differ in number of lines"
    errors <- c(errors, msg)
  }

  #Ensure proper class
  ##Yield column is numeric
  logicalNumber_yield <- all(sapply(object@yield, is, class = "numeric"))
  if(!logicalNumber_yield){
    msg <- "Specified column of yield is not of class numeric"
    errors <- c(errors, msg)
  }

  ##X columns are numeric
  logicalNumber_X <- all(sapply(object@X, is, class = "numeric"))
  if(!logicalNumber_X){
    msg <- "Specified columns of X are not all of class numeric"
    errors <- c(errors, msg)
  }

  #Ensure yield possess no NA
  if(any(is.na(object@yield))){
    msg <- "Yield possesses NA"
    errors <- c(errors, msg)
  }
  #Ensure X possess no NA
  if(any(is.na(object@X))){
    msg <- "X possesses NA"
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
#' @details The function \code{CndData} should be used to build an object. Both arguments \code{yield} and \code{label} can be provided as vectors, they will be coerced into \code{data.frame}. If provided as vector, the column name of \code{label} would be "label", and the one \code{yield} would be "yield". For the latter, the column name would also be overwriten as "yield" if the entry is a \code{data.frame}. If empty, the number of rows of \code{label} would be matched to those of \code{yield} and \code{X} at initialisation.
#' @rdname S4Class-CndData
#' @export
#'
CndData = function(yield = data.frame(yield = numeric(0)), X = data.frame(), label = data.frame()){
  #Generate the object of class CndData and check
  data <- new("CndData", yield = yield, X = X, label = label)
  return(data)
}
#--------------------------------------------------------------------
setMethod("initialize", "CndData", function(.Object,  yield = numeric(0), X = data.frame(), label = data.frame(), ...) {

  #Define the yield slot
  if(!is(yield, "data.frame")){
    ##Conversion of yield into a data.frame and attribution
    .Object@yield <- data.frame(yield = yield)
  }else{
    ##Ensure the column name is yield (only cover ncol = 1, will be prevented otherwise later)
    if(ncol(yield) == 1){
      colnames(yield) = "yield"
    }
    ##Attribution
    .Object@yield <- yield
  }

  #Define the X slot
  .Object@X <- X

  #Define the label slot
  logicalEmptyLabel <- (is(label, "data.frame") & all(dim(label) == c(0,0))) | length(label) == 0
  if(logicalEmptyLabel){
    #Case with no label provided
    M <- matrix(1, ncol = 0, nrow = nrow(.Object@X))
    .Object@label <- as.data.frame(M)
  }else if(is(label, "data.frame")){
    #Case with label as data.frame
    .Object@label <- label
  }else{
    #Case with label as vector
    .Object@label <- data.frame(label = label)
  }

  #.Object <- callNextMethod(.Object)
  #Validate and return object
  validObject(.Object)
  return(.Object)
})
#--------------------------------------------------------------------
