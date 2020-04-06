#' @title Landry's method
#' @description A predefined method developped by Landry in the context of the development of the Quebec's fertilization reference charts. Some flexibility is provided on the parameter values to consider during the analysis.
#' @param dropNutrient (transformation) the nutrient to drop (column name of the slot \code{X}) after the clr transformation.
#' @param percent (subset) the percentage of observation to include in the high yield subpopulation.
#' @param nSamp (norm) the number of subsets used for initial estimates (\code{integer}) or the name of one of the method: \code{best}, \code{exact}, \code{deterministic}. See the function \code{covMcd} from the \code{robustbase} package for more details on these methods.
#' @param n_group,min_group_x,min_group_y,labelName (analysis) arguments to pass to the function \code{cndCateNelson}: number of groups, minimum number of values per group in the x and y partioning, label's column name to use for the Cate-Nelson analysis.
#' @details The steps of the predefined method, and some elements of context, are as follow:
#' \describe{
#'  \item{\code{transformation}}{to normalize the data, \code{X} is first transformed as clr (function \code{cndClr}). However, because the this transformation makes the covariance matrix is not invertible, a condition required to compute the Mahalanobis distance, a nutrient is dropped after the transformation. The \code{fill} component is used by default, but any nutrient can be chosen by precising \code{dropNutrient} as an entry of the \code{cndMethodLandry} function. The approach was preconised by Parent et al. (2009).}
#'  \item{\code{subset}}{a high yield subpopulation is then identified, using the function \code{cndCutoff}, at a percentage specified by the parameter \code{percent}. By default, the highest 25 percent in yield make this subset. Using a fixed percentage to define the high yield subpopulation was brought by De Bauw et al. (2016) to circumvent estimation problems in the approach developped by Khiari et al. (2001) and used in Parent el al. (2009).}
#'  \item{\code{norm}}{a robust norm is then obtained through the minimum covariance determinant estimation (function \code{cndMcd}). The returned object is of class \code{McdNorm}, which inherits from the class \code{CndNorm}. The slot \code{subset} of the \code{McdNorm} object (representing further subseting by the MCD) can be used to associate a yield to the norm. However, this yield might not be representative for another dataset. The method, brought by Landry, replaces the iterative method of outlier detection used by Parent el al. (2009) to eliminate unbalanced cases that generate high yield (e.g. plants that grew in soils rich enough to reach maximum yield might be less influenced by balances).}
#'  \item{\code{distance}}{the Mahalanobis squared distance (function \code{cndMahalanobis}) is then calculated, using the reference's norm, for the reference's data and on a new data set in the analysis. The distance was proposed by Parent et al. (2009), as an improvement from Khiari (2001), to eliminate colinearity.}
#'  \item{\code{analysis}}{a Cate-Nelson analysis is then performed on the yield (y) and the squared distance (x), instead of a single element concentration. The method was used in both Khiari et al. (2001) and Parent et al. (2009) to classify points according to multiple elements at once, and might serve to delimit the yield and critical distance of observations near the centroid for data collected under new conditions.}
#' }
#' @section References:
#' De Bauw P, Van Asten P, Jassogne L, Merckx R. 2016. Soil fertility gradients and production constraints for coffee and banana on volcanic mountain slopes in the East African Rift: A case study of Mt. Elgon. Agric. Ecosyst. Environ. 231: 166-175.\cr \cr
#' Khiari L, Parent L-E, Tremblay N. 2001. Selecting the high-yield subpopulation for diagnosing nutrient imbalance in crops. Agron. J. 93(4): 802-808.\cr \cr
#' Parent L-E, Natale W, Ziadi N. 2009. Compositional nutrient diagnosis of corn using the Mahalanobis distance as nutrient imbalance index. Can. J. Soil Sci. 89(4): 383-390. \cr \cr
#' @export
#' @examples
#' #Default method
#' method1 <- cndMethodLandry()
#'
#' #The method with specific parameters
#' method2 <- cndMethodLandry(percent = 50, nSamp = 2000)
#'
cndMethodLandry <- function(dropNutrient = "fill", percent = 25, nSamp = 1000, n_group = 2, min_group_x = 2, min_group_y = 1, labelName = NULL){
  transformation <- CndCall("cndClr", list(dropNutrient = dropNutrient))
  subset         <- CndCall("cndCutoff", args = list(method = "percent", param = percent))
  norm           <- CndCall("cndMcd", args = list(nSamp = nSamp))
  distance       <- CndCall("cndMahalanobis", args = list())
  analysis       <- CndCall("cndCateNelson", args = list(min_group_x = min_group_x, min_group_y = min_group_y, labelName = labelName))
  cndMethod      <- CndMethod(transformation = transformation, subset = subset, norm = norm, distance = distance, analysis = analysis)
  return(cndMethod)
}
