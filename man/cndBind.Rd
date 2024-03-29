% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cndBind.R
\name{cndBind}
\alias{cndBind}
\title{Bind CndData}
\usage{
cndBind(cndData)
}
\arguments{
\item{cndData}{An object of class \code{CndData} or \code{CndDataAugmented}.}
}
\value{
Return a single \code{data.frame}.
}
\description{
Bind slots of a \code{CndData}, or a \code{CndDataAugmented}, object into a single \code{data.frame}. For the latter, only the slot \code{other} is combined to the slots \code{X}, \code{Y} and \code{label}; the slot \code{suppl} is not used.
}
\examples{
#' #General example
##Generate objects of classes CndData and CndDataAugmented
n = 20
yield <- rnorm(n)
X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
data <- CndData(yield = yield, X = X)
dataAugmented <- cndAugment(data, other = X, suppl = list(comment = "comment"))

##Bind elements into a data.frame for each class
cndBind(data)
cndBind(dataAugmented)

}
