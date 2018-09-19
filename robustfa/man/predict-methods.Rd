\name{predict-methods}
\docType{methods}
\alias{predict-methods}
\alias{predict,Fa-method}
\alias{predict}

\title{Calculates prediction}

\description{
Calculates prediction using the results in object. The newdata argument is an optional data frame or matrix in which to look for variables with which to predict. If newdata is omitted, the scores are used.
}

\usage{
predict(object, ...)
} 

\arguments{

    \item{object}{an object of class \code{"Fa"} or of a class derived from \code{"Fa"}}

    \item{...}{additional arguments, e.g., newdata: an optional data frame or matrix in which to look for variables with which to predict. If newdata is not missing, newdata should be scaled before \code{"predict"}.}

}

\section{Methods}{
\describe{

\item{\code{signature(object = "Fa")}}{
generic functions - see \code{show}, \code{print}, \code{summary}, \code{predict}, \code{plot}, \code{getCenter}, \code{getEigenvalues}, \code{getFa}, \code{getLoadings}, \code{getQuan}, \code{getScores}, \code{getSdev}
}

}}

\references{
Zhang, Y. Y. (2013), An Object Oriented Solution for Robust Factor Analysis.
}

\author{ Ying-Ying Zhang (Robert) \email{robertzhangyying@qq.com} }

\examples{

data("hbk")
hbk.x = hbk[,1:3] 

faCovPcaRegMcd = FaCov(x = hbk.x, factors = 2, method = "pca",
scoresMethod = "regression", cov.control = CovControlMcd()); faCovPcaRegMcd

## If missing newdata, the scores are used
predict(faCovPcaRegMcd)

##
## If not missing newdata, newdata should be scaled first.
##
newdata = hbk.x[1, ]
cor = FALSE # the default
newdata = { 
if (cor == TRUE)
	# standardized transformation
	scale(newdata, center = faCovPcaRegMcd@center, 
	scale = sqrt(diag(faCovPcaRegMcd@covariance)))
else # cor == FALSE
	# centralized transformation
	scale(newdata, center = faCovPcaRegMcd@center, scale = FALSE)
}

##
## Now, prediction = predict(faCovPcaRegMcd)[1,] = faCovPcaRegMcd@scores[1,]
##
prediction = predict(faCovPcaRegMcd, newdata = newdata)
prediction

}

\keyword{methods}
