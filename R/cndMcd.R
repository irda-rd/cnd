#' @title Minimum covariance determinant
#' @description Compute a robust norm (location and scatter) estimate via the minimum covariance determinant (MCD), on the \code{X} component of \code{cndData}. The function is a wrapper of the function \code{covMcd} from the package \code{robustbase}.
#' @param cndData an object of class \code{CndData}.
#' @param nSamp the number of subset used for initial estimates (\code{integer}) or the name of one of the method: \code{best}, \code{exact}, \code{deterministic}. See the function \code{covMcd} for more details on these methods.
#' @param ... other paramaters to pass to the function \code{covMcd} from the package \code{robustbase}.
#' @return Return an object of class \code{McdNorm} that contains the same slots as \code{CndNorm}, but also the logarithm of the covariance matrix determinant (slot \code{logDet}) for the best subset (slot \code{subset}, returned as an object of class \code{CndData}).
#' @import robustbase
#' @export
#' @examples
#' #Generate an object of class CndData.
#' n <- 20
#' yield <- 100+rnorm(n)
#' X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
#' cndData <- CndData(yield = yield, X = X)
#' transfData <- cndClr(cndData, dropNutrient = "x3")
#'
#' #Compute the MCD norm
#' cndMcd(transfData, nSamp = 1000)
#'
setGeneric("cndMcd", function(cndData, nSamp, ...) standardGeneric("cndMcd"))
setMethod("cndMcd", signature(cndData = "CndData"), function(cndData, nSamp, ...) {

  #Verification de recommended conditions
  m <- dim(cndData@X)[1]
  p <- dim(cndData@X)[2]
  if(m <= 5*p){
    warning("The number of rows of X (m) for the mcd is recommended to respect m > 5p, with p the number of columns")
  }

  #Computation of mcd
  mcd <- covMcd(x = cndData@X, nsamp = nSamp)
  logDet <- mcd$crit
  
  #Acquire subsets
  logicalSubsetRaw <- seq_len(m) %in% mcd$best
  subsetRaw <- cndSubsetData(cndData, logicalSubsetRaw)
  
  logicalSubsetReweighted <- mcd$raw.weights == 1
  subsetReweighted <- cndSubsetData(cndData, logicalSubsetReweighted)

  #Acquire the norm
  location <- mcd$center #Correspond to apply(subset_reweighted@X,MARGIN = 2, mean)
  scatter <- mcd$cov #Correspond to mcd$cnp2[1]*mcd$cnp2[2]*cov(subset_reweighted@X)

  norm <- McdNorm(location = location, scatter = scatter, logDet = logDet, subsetRaw = subsetRaw, subsetReweighted = subsetReweighted)
  return(norm)
})
