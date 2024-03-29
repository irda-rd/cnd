% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4Class-McdNorm.R
\docType{class}
\name{S4Class-McdNorm}
\alias{S4Class-McdNorm}
\alias{McdNorm}
\alias{class:McdNorm}
\alias{McdNorm-class}
\alias{setValidity_McdNorm}
\title{CND Minimum covariance determinant estimates (S4 Class)}
\usage{
McdNorm(location = numeric(0), scatter = matrix(numeric(0), ncol = 0,
  nrow = 0), logDet = numeric(0), subsetRaw = CndData(),
  subsetReweighted = CndData())
}
\description{
S4 class that extend the class \code{CndNorm} to also include the logarithm of the covariance matrix determinant \code{logDet} associated to the robust method and subsets involved in the calculation of the location and scatter.
}
\details{
The validity method check if: \code{location} and \code{scatter} respect validity checks defined for \code{CndNorm} and if \code{logDet} only possess one value.

The function \code{McdNorm} can be used to build an object.
}
\section{Slots}{

\describe{
\item{\code{location}}{a \code{numeric}, location estimate.}

\item{\code{scatter}}{a \code{matrix}, variance or covariance matrix estimate.}

\item{\code{logDet}}{a \code{matrix}, logarithm of the determinant of the covariance matrix, which was minimized in the mcd.}

\item{\code{subsetRaw}}{a \code{CndData} object, representing the subset of observation that minimized the determinant.}

\item{\code{subsetReweighted}}{a \code{CndData} object, representing the subset of observation resulting from the reweighting.}
}}

\examples{
#Examine slots for the class
getSlots("McdNorm")

#General example
##Define elements provided in by the mcd
###The norm and logDet
location <- 1:3
scatter <- matrix(1:9,3,3)
logDet <- 1

##A data subset (here some CndData object)
n <- 10
yield <- rnorm(n)
X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
label <- LETTERS[(seq_len(n)-1)\%\%4+1]
subset <- CndData(yield = yield, X = X, label = label)

##Generate an object of class McdNorm
norm <- McdNorm(location = location, scatter = scatter,logDet = logDet, subsetRaw = subset, subsetReweighted = subset)

}
