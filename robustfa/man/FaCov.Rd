\name{FaCov}
\alias{FaCov}
\alias{FaCov.formula}
\alias{FaCov.default}

\title{
Robust Factor Analysis
}
\description{
Robust factor analysis are obtained by replacing the classical covariance matrix 
by a robust covariance estimator. This can be one of the available estimators in \code{rrcov}, i.e., MCD, OGK, M, S, SDE, or MVE estimator.
}
\usage{
FaCov(x, \dots)
\method{FaCov}{formula}(formula, data = NULL, factors = 2, cor = FALSE, method = "mle", 
scoresMethod = "none", \dots)
\method{FaCov}{default}(x, factors = 2, cor = FALSE, cov.control = CovControlMcd(), 
method = c("mle", "pca", "pfa"), 
scoresMethod = c("none", "regression", "Bartlett"), \dots)
}

\arguments{

  \item{x}{A formula or a numeric matrix or an object that can be coerced to a numeric matrix.}

  \item{\dots}{Arguments passed to or from other methods.}

  \item{formula}{A formula with no response variable, referring only to numeric variables.}

  \item{data}{An optional data frame (or similar: see \code{\link{model.frame}}) containing the variables in the \code{formula}.}

  \item{factors}{The number of factors to be fitted.}

  \item{cor}{A logical value indicating whether the calculation should use the covariance matrix (\code{cor = FALSE}) or the correlation matrix (\code{cor = TRUE}).
}

  \item{method}{The method of factor analysis, one of "mle" (the default), "pca", and "pfa".}

  \item{scoresMethod}{Type of scores to produce, if any. The default is \code{"none"}, \code{"regression"} gives Thompson's scores, \code{"Bartlett"} gives Bartlett's weighted least-squares scores.}

  \item{cov.control}{Specifies which covariance estimator to use by providing a \code{\link{CovControl-class}} object. The default is \code{\link{CovControlMcd-class}} which will indirectly call \code{\link{CovMcd}}. If \code{cov.control=NULL} is specified, the classical estimates will be used by calling \code{\link{CovClassic}}.}

}

\details{
\code{FaCov}, serving as a constructor for objects of class \code{\link{FaCov-class}} is a generic function with "formula" and "default" methods. 
}

\value{
An S4 object of class \code{\link{FaCov-class}} which is a subclass of the virtual class \code{\link{Fa-class}}.    
}

\references{
Zhang, Y. Y. (2013), An Object Oriented Solution for Robust Factor Analysis.
}

\author{ Ying-Ying Zhang (Robert) \email{robertzhangyying@qq.com} }

\seealso{
\code{\link{FaClassic-class}}, \code{\link{FaCov-class}}, \code{\link{FaRobust-class}}, \code{\link{Fa-class}}
}

\examples{

data("hbk")
hbk.x = hbk[,1:3] 

##
## faCovPcaRegMcd is obtained from FaCov.default
##
faCovPcaRegMcd = FaCov(x = hbk.x, factors = 2, method = "pca",
scoresMethod = "regression", cov.control = CovControlMcd()); faCovPcaRegMcd

##
## In fact, it is equivalent to use FaCov.formula
## faCovForPcaRegMcd = faCovPcaRegMcd
##
faCovForPcaRegMcd = FaCov(~., data = as.data.frame(hbk.x), 
factors = 2, method = "pca", scoresMethod = "regression", 
cov.control = CovControlMcd()); faCovForPcaRegMcd

}

\keyword{robust}
