\name{computeScores}
\alias{computeScores}

\title{
Compute Factor Scores
}
\description{
Compute factor scores on the result of factor analysis method, the method is one of "mle", "pca", and "pfa". 
}
\usage{
computeScores(out, x = data, covmat = covmat, cor = cor, scoresMethod = scoresMethod)
}

\arguments{

  \item{out}{
The result of factorScorePca(), factorScorePfa(), or factanal(). It is a list.
}
  \item{x}{
A numeric matrix.
}
  \item{covmat}{
A list with components: cov, center, and n.obs.
}
  \item{cor}{
A logical value indicating whether the calculation should use the covariance matrix (\code{cor = FALSE}) or the correlation matrix (\code{cor = TRUE}).
}
  \item{scoresMethod}{
Type of scores to produce, if any. The default is \code{"none"}, \code{"regression"} gives Thompson's scores, \code{"Bartlett"} gives Bartlett's weighted least-squares scores.
}
}

%% \details{}

\value{
The output is a list. Except for the components of \code{out}, it also has components:

\item{scoringCoef }{The scoring coefficients.}

\item{scores }{The matrix of scores.}

\item{meanF }{The sample mean of the scores.}

\item{corF }{The sample correlation matrix of the scores.}

\item{eigenvalues }{The eigenvalues of the running matrix.}

\item{covariance }{The covariance matrix.}

\item{correlation }{The correlation matrix.}

\item{usedMatrix }{The used matrix (running matrix) to compute \code{scoringCoef} etc..}

\item{reducedCorrelation }{NULL. The reduced correlation matrix, reducedCorrelation is calculated in factorScorePfa.R.}

scoringCoef = F = meanF = corF = NULL if \code{scoresMethod = "none"}.
}

\references{
Zhang, Y. Y. (2013), An Object Oriented Solution for Robust Factor Analysis.
}

\author{ Ying-Ying Zhang (Robert) \email{robertzhangyying@qq.com} }

%% \seealso{}

\examples{
data("stock611")
stock604 = stock611[-c(92,2,337,338,379,539,79), ]
data = as.matrix(stock604[, 3:12])

factors = 2
cor = TRUE
scoresMethod = "regression" 

covx = Cov(data)
covmat = list(cov = getCov(covx), center = getCenter(covx), n.obs = covx@n.obs)

out = factanal(factors = factors, covmat = covmat)

out = computeScores(out, x = data, covmat = covmat, cor = cor, scoresMethod = scoresMethod)
out

}

\keyword{robust}
