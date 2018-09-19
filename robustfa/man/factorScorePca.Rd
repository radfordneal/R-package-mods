\name{factorScorePca}
\alias{factorScorePca}

\title{
Factor Analysis by Principal Component Analysis (PCA)
}
\description{
Perform principal component factor analysis on a covariance matrix or data matrix. 
}
\usage{
factorScorePca(x, factors = 2, covmat = NULL, cor = FALSE, 
rotation = c("varimax", "none"), 
scoresMethod = c("none", "regression", "Bartlett"))
}

\arguments{
  \item{x}{
A numeric matrix or an object that can be coerced to a numeric matrix.
}
  \item{factors}{
The number of factors to be fitted.
}
  \item{covmat}{
A covariance matrix, or a covariance list as returned by \code{\link{cov.wt}}. Of course, correlation matrices are covariance matrices.
}
  \item{cor}{
A logical value indicating whether the calculation should use the covariance matrix (\code{cor = FALSE}) or the correlation matrix (\code{cor = TRUE}).
}
  \item{rotation}{
character. "none" or "varimax": it will be called with first argument the loadings matrix, and should return a list with component \code{loadings} giving the rotated loadings, or just the rotated loadings.
}
  \item{scoresMethod}{
Type of scores to produce, if any. The default is \code{"none"}, \code{"regression"} gives Thompson's scores, \code{"Bartlett"} gives Bartlett's weighted least-squares scores.
}
}

\details{
Other feasible usages are:

\code{factorScorePca(factors, covmat)}

\code{factorScorePca(x, factors, rotation, scoresMethod)}

If \code{x} is missing, then the following components of the result will be NULL: scores, ScoringCoef, meanF, corF, and n.obs.
}

\value{
An object of class \code{"factorScorePca"} with components:

\item{call }{The matched call.}

\item{loadings }{A matrix of loadings, one column for each factor. This is of class \code{"loadings"} if \code{rotation = "varimax"}: see \code{\link{loadings}} for its print method; It is a plain matrix if \code{rotation = "none"}.}

\item{communality }{The common variance.}

\item{uniquenesses }{The uniquenesses/specific variance computed.}

\item{covariance }{The robust/classical covariance matrix.}

\item{correlation }{The robust/classical correlation matrix.}

\item{usedMatrix }{The used matrix (running matrix). It may be the covariance or correlation matrix according to the value of \code{cor}.}

\item{reducedCorrelation }{NULL. The reduced correlation matrix, reducedCorrelation is calculated in factorScorePfa.R.}

\item{factors }{The argument factors.}

\item{method }{The method: always \code{"pca"}.}

\item{scores }{If requested, a matrix of scores. NULL if \code{x} is missing.}

\item{scoringCoef }{The scoring coefficients. NULL if \code{x} is missing.}

\item{meanF }{The sample mean of the scores. NULL if \code{x} is missing.}

\item{corF }{The sample correlation matrix of the scores. NULL if \code{x} is missing.}

\item{scoresMethod }{The argument \code{scoresMethod}.}

\item{n.obs }{The number of observations if available. NULL if \code{x} is missing.}

\item{center }{The center of the data. }

\item{eigenvalues }{The eigenvalues of the usedMatrix.}

}

\references{
Zhang, Y. Y. (2013), An Object Oriented Solution for Robust Factor Analysis.
}

\author{ Ying-Ying Zhang (Robert) \email{robertzhangyying@qq.com} }

\seealso{
\code{\link{factorScorePfa}}, \code{\link{factanal}}
}

\examples{

data(stock611)
R611=cor(stock611[,3:12]); R611

## covmat is a matrix
fsPca1=factorScorePca(factors = 3, covmat = R611); fsPca1

## covmat is a list
covx <- Cov(stock611[,3:12])
covmat <- list(cov=getCov(covx), center=getCenter(covx), n.obs=covx@n.obs)
fsPca2=factorScorePca(factors = 3, covmat = covmat); fsPca2

## fsPca3 contains scores etc.
fsPca3=factorScorePca(x = stock611[,3:12], factors = 2, cor = TRUE, rotation = "varimax", 
scoresMethod = "regression"); fsPca3

}

\keyword{robust}
