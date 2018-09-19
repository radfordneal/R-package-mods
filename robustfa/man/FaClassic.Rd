\name{FaClassic}
\alias{FaClassic}
\alias{FaClassic.formula}
\alias{FaClassic.default}

\title{
Classical Factor Analysis
}
\description{
Performs a classical factor analysis and returns the results as an object of class \code{"FaClassic"} (a.k.a. constructor).
}
\usage{
FaClassic(x, ...)
\method{FaClassic}{formula}(formula, data = NULL, factors = 2, cor = FALSE, method = "mle", 
scoresMethod = "none", \dots)
\method{FaClassic}{default}(x, factors = 2, cor = FALSE, method = c("mle", "pca", "pfa"), 
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

}

%\details{}

\value{
An S4 object of class \code{\link{FaClassic-class}} which is a subclass of the virtual class \code{\link{Fa-class}}.    
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

## faClassicPcaReg uses the default method
faClassicPcaReg = FaClassic(x = hbk.x, factors = 2, method = "pca",
scoresMethod = "regression"); faClassicPcaReg
summary(faClassicPcaReg)

## faClassicForPcaReg uses the formula interface
## faClassicForPcaReg = faClassicPcaReg
faClassicForPcaReg = FaClassic(~., data=as.data.frame(hbk.x), factors = 2, 
method = "pca", scoresMethod = "regression"); faClassicForPcaReg
summary(faClassicForPcaReg)

}

\keyword{robust}
