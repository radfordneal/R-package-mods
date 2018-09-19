\name{FaRobust-class}
\Rdversion{1.1}
\docType{class}
\alias{FaRobust-class}

\title{Class \code{"FaRobust"}}
\description{
Class \code{"FaRobust"} is a virtual base class for all robust FA classes. Currently the only available robust FA class is \code{"FaCov"}. The class \code{"FaRobust"} serves as a base class for deriving all other classes representing the results of the robust Factor Analysis methods.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
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
Class \code{"\linkS4class{Fa}"}, directly.
}
\section{Methods}{
No methods defined with class "FaRobust" in the signature.
}

\references{
Zhang, Y. Y. (2013), An Object Oriented Solution for Robust Factor Analysis.
}

\author{ Ying-Ying Zhang (Robert) \email{robertzhangyying@qq.com} }


\seealso{
\code{\link{FaClassic-class}}, \code{\link{FaCov-class}}, \code{\link{FaRobust-class}}, \code{\link{Fa-class}}
}

\examples{
showClass("FaRobust")
}
\keyword{classes}
