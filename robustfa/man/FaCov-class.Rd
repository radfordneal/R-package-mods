\name{FaCov-class}
\Rdversion{1.1}
\docType{class}
\alias{FaCov-class}

\title{Class \code{"FaCov"}}

\description{
Robust FA based on a robust covariance matrix. Robust FA are obtained by replacing the classical covariance matrix by a robust covariance estimator. This can be one of the available in \code{rrcov} estimators, i.e., MCD, OGK, M, S, SDE, or MVE estimator.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("FaCov", ...)}.
But the usual way of creating \code{FaCov} objects is a call to the function \code{FaCov} which serves as a constructor.
}

\section{Slots}{
  \describe{
    \item{\code{call}:}{Object of class \code{"language"} an unevaluated function call}
    \item{\code{converged}:}{Object of class \code{"Ulogical"} a logical character indicates whether the iterations converged }
    \item{\code{loadings}:}{Object of class \code{"matrix"} the matrix of variable loadings }
    \item{\code{uniquenesses}:}{Object of class \code{"vector"} the uniquenesses computed }
    \item{\code{covariance}:}{Object of class \code{"matrix"} the covariance matrix }
    \item{\code{correlation}:}{Object of class \code{"matrix"} the correlation matrix }
    \item{\code{usedMatrix}:}{Object of class \code{"matrix"} the used matrix (running matrix) }
    \item{\code{criteria}:}{Object of class \code{"Unumeric"}. The results of the optimization: the value of the negative log-likelihood and information on the iterations used. }
    \item{\code{factors}:}{Object of class \code{"numeric"} the number of factors }
    \item{\code{dof}:}{Object of class \code{"Unumeric"}. The number of degrees of freedom of the factor analysis model. }
    \item{\code{method}:}{Object of class \code{"character"}. The method: one of "mle", "pca", and "pfa".}
    \item{\code{scores}:}{Object of class \code{"Umatrix"}. If requested, a matrix of scores. }
    \item{\code{scoresMethod}:}{Object of class \code{"character"}. The scores method: one of "none", "regression", and "Bartlett". }
    \item{\code{STATISTIC}:}{Object of class \code{"Unumeric"}. The significance-test statistic, if it can be computed. }
    \item{\code{PVAL}:}{Object of class \code{"Unumeric"}. The significance-test P value, if it can be computed. }
    \item{\code{n.obs}:}{Object of class \code{"Unumeric"}. The number of observations if available. }
    \item{\code{center}:}{Object of class \code{"Uvector"}. The center of the data. }
    \item{\code{eigenvalues}:}{Object of class \code{"vector"} the eigenvalues }
    \item{\code{cov.control}:}{Object of class \code{"UCovControl"}. Record the cov control method. }
  }
}

\section{Extends}{
Class \code{"\linkS4class{FaRobust}"}, directly.
Class \code{"\linkS4class{Fa}"}, by class "FaRobust", distance 2.
}

\section{Methods}{
No methods defined with class "FaCov" in the signature.
}

\references{
Zhang, Y. Y. (2013), An Object Oriented Solution for Robust Factor Analysis.
}

\author{ Ying-Ying Zhang (Robert) \email{robertzhangyying@qq.com} }


\seealso{
\code{\link{FaClassic-class}}, \code{\link{FaCov-class}}, \code{\link{FaRobust-class}}, \code{\link{Fa-class}}
}

\examples{
showClass("FaCov")
}

\keyword{classes}
