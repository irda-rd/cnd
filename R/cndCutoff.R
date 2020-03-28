#' @title Yield cutoff subsetting
#' @description Subset cndData on the basis of a minimum yield value, either absolute or as a percentage.
#' @param cndData an object of class \code{CndData}.
#' @param method \code{character}, defining the basis on which to subset data. Either \code{percent} or \code{value}.
#' @param param \code{numeric}, specifying either the percentage of observertaion to keep (if \code{method = percent}), or the yield value delimiting the high yield subpopulation (if \code{method = value}).
#' @details Only the number of observation encompassed by the percentage would be considered.
#' For example, with 10 observations and a percentage to preserve of 25 percent, only the first two highest yield observations would be kept.
#' For both method, equality is considered as part of the high yield subset (e.g. observation with a yield of 10000 would be kept in the subset if the cutoff value is also 10000).
#' @return Return a subsetted \code{cndData} object.
#' @export
#' @examples
#' #General data
#' ##CndData
#' n <- 20
#' yield <- 100 + rnorm(n)
#' X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
#' label <- LETTERS[(seq_len(n)-1)%%4+1]
#' cndData <- CndData(yield = yield, X = X, label = label)
#'
#' ##Cutoff the data according to yield
#' cndCutoff(cndData, method = "percent", param = 50)
#'
setGeneric("cndCutoff", function(cndData, method , param) standardGeneric("cndCutoff"))
setMethod("cndCutoff", signature(cndData = "CndData"), function(cndData, method, param) {
  #Identify cutoff value that define the high yield subpopulation
  if(method == "percent"){
    ##Define the yield cutoff using the percentage method. The provided percentage correspond to the high yield subpopulation)
    yieldCutoff <- cutoffPercent(as.vector(t(cndData@yield)), param)
  }else if(method == "value"){
    ##Define the yield cutoff as a known value
    yieldCutoff = param
  }else{
    stop("method should either be percent or value")
  }
  logicalSubset = as.vector(cndData@yield >= yieldCutoff)
  subsetData <- cndSubsetData(cndData, logicalSubset)
  return(subsetData)
})

cutoffPercent = function(yield, percent){
  #Percent correspond to the proportion of yield to be included in the high yield sub-population
  L = length(yield)
  yield <- sort(yield, decreasing = TRUE)
  yieldCutoff = yield[floor(percent/100*L)]
  return(yieldCutoff)
}


