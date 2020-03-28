#' @title Mahalanobis distance
#' @description Compute the Mahalanobis squared distance on the \code{X} slot of \code{cndData}, using \code{location} and \code{scatter} of \code{cndNorm}. The function is a wrapper of the function \code{mahalanobis} from the package \code{stats}.
#' @param cndData an object of class \code{CndData}.
#' @param cndNorm an object of class \code{CndNorm}.
#' @param ... other parameters to pass to the function \code{mahalanobis}.
#' @return Return \code{cndData} augmented with the squared distance as a data.frame coulmn under the slot \code{other} (i.e. the returned object is of class CndDataAugmented).
#' @export
#' @examples
#' #General data
#' ##CndData
#' n <- 50L
#' yield <- 100+rnorm(n)
#' X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
#' label <- LETTERS[(seq_len(n)-1)%%4+1]
#' cndData <- CndData(yield = yield, X = X, label = label)
#' transfData <- cndIlr(cndData)
#'
#' ##CndNorm
#' cndNorm <- cndMcd(transfData, nSamp = 1000)
#'
#' #Computing the distance
#' cndMahalanobis(transfData, cndNorm)
#'
setGeneric("cndMahalanobis", function(cndData, cndNorm, ...) standardGeneric("cndMahalanobis"))
setMethod("cndMahalanobis", signature(cndData = "CndData", cndNorm = "CndNorm"), function(cndData, cndNorm, ...) {
  msd <- stats::mahalanobis(cndData@X, center = cndNorm@location, cov = cndNorm@scatter, ...)
  msd <- data.frame(distance = msd, stringsAsFactors = FALSE)
  cndData <- cndAugment(cndData, other = msd)
  return(cndData)
})
