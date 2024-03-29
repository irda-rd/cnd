% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4Class-CndDataAugmented.R
\docType{class}
\name{S4Class-CndDataAugmented}
\alias{S4Class-CndDataAugmented}
\alias{CndCndDataAugmented}
\alias{class:CndDataAugmented}
\alias{CndDataAugmented-class}
\alias{setValidity_CndDataAugmented}
\alias{CndDataAugmented}
\title{CND Data Augmented (S4 class)}
\usage{
CndDataAugmented(yield = data.frame(yield = numeric(0)),
  X = data.frame(), label = data.frame(), other = data.frame(),
  suppl = list())
}
\description{
S4 class that contains information on the data to use for the cnd analysis, but that also contains additional fields.
}
\details{
The validity method check if: \code{yield} only possesses one column; \code{yield}, \code{X}, \code{label} and \code{other} have the same number of lines; columns of \code{yield} and \code{X} are of class \code{numeric}.

The function \code{CndDataAugmented} should be used to build an object. Both arguments \code{yield} and \code{label} can be provided as vectors, they will be coerced into \code{data.frame}. If provided as vector, the column name of \code{label} would be "label", and the one \code{yield} would be "yield". For the latter, the column name would also be overwriten as "yield" if the entry is a \code{data.frame}. If empty, the number of rows of \code{label} would be matched to those of \code{yield} and \code{X} at initialisation.
}
\section{Slots}{

\describe{
\item{\code{yield}}{\code{data.frame}, with one column, of yield associated to lines of \code{X}.
The entry can be a \code{numeric} vector if the object is built using the function \code{CndData}.}

\item{\code{X}}{\code{data.frame} of composition, with columns corresponding to nutrients and lines to samples.}

\item{\code{label}}{(optional) \code{data.frame} of labels associated to lines of \code{X}. By default, an empty \code{data.frame} with the same number of rows as \code{yield} and \code{X}, but no columns.}

\item{\code{other}}{a \code{data.frame} of additional values to associate with points of \code{cndData}, it must possess the same number of lines than \code{yield} and \code{X}.}

\item{\code{suppl}}{\code{list} of supplementary material to pass along the object.}
}}

\examples{
#General example
#' ##Observe slots of CndMethod
getSlots("CndDataAugmented")

##Generate data for the example
n <- 20
yield <- rnorm(n)
X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
label <- LETTERS[(seq_len(n)-1)\%\%4+1]

##Generate an object of class CndDataAugmented
data <- CndDataAugmented(yield = yield, X = X, label = label,
other = X, suppl = list(comment = "some comment"))

}
