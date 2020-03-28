#' @title Cate Nelson analysis
#' @description Perform a Cate Nelson analysis on a \code{CndDataAugmented} object. The function is a wrapper of the function \code{cate_nelson} from the \code{catenelson} package. The \code{yield} slot is used as \code{y}, and the column \code{distance} of the slot \code{other} is used as \code{x}.
#' @param cndData An object of class \code{CndDataAugmented}, with a column named \code{distance} under the slot \code{other}, representing the squared distance from the centroid.
#' @param labelName The column name of the  \code{cndData}'s \code{label} slot to be use as the argument label in the function \code{cate_nelson}.
#' @param n_group,trend,x_lab,y_lab \code{cate_nelson}'s parameter with imposed default values specific to cnd.
#' @param ... Other arguments to pass to the function \code{cate_nelson}.
#' @import catenelson
#' @export
#' @examples
#' #General data
#' ##CndData
#' n <- 20
#' yield <- 100+rnorm(n)
#' X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
#' label <- LETTERS[(seq_len(n)-1)%%4+1]
#' cndData <- CndData(yield = yield, X = X, label = label)
#' transfData <- cndIlr(cndData)
#' ##CndNorm and distance (CndDataAugmented)
#' cndNorm <- cndMcd(transfData, nSamp = 1000)
#' distance <- cndMahalanobis(transfData, cndNorm)
#'
#' ##Perform a Cate-Nelson analysis
#' cndCateNelson(distance, label = "label")
#'
setGeneric("cndCateNelson", function(cndData, n_group = 2, trend = "negative", labelName = NULL, x_lab = "Squared distance", y_lab = "Yield", ...) standardGeneric("cndCateNelson"))
setMethod("cndCateNelson", signature(cndData = "CndDataAugmented"), function(cndData, n_group = 2, trend = "negative", labelName = NULL, x_lab = "Squared distance", y_lab = "Yield", ...) {

  #Define the label column from the cndData object to use in the cate nelson graph
  if(is.null(labelName)){
    label <- NULL
  }else if(length(labelName) == 1){
    label <- cndData@label[,labelName]
  }else{
    stop("labelName must be of length one and correspond to a column name of the label slot of the cndData ")
  }

  CN = cate_nelson(x = cndData@other$distance, y = cndData@yield$yield, label = label, n_group = n_group, trend = trend,
                   x_lab = x_lab, y_lab = y_lab, ...)
  cndData <- cndAugment(cndData, suppl = list(cateNelson = CN))

  return(cndData)
})
