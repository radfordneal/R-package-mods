\name{Fa-class}
\Rdversion{1.1}
\docType{class}
\alias{Fa-class}

\title{Class \code{"Fa"}}

\description{
Class \code{"Fa"} is a virtual base class for all classical and robust FA classes. \code{"Fa"} searves as a base class for deriving all other classes representing the results of the classical and robust Factor Analysis methods.
}

\section{Objects from the Class}{A virtual Class: No objects may be created from it.}

\section{Slots}{
  \describe{
    \item{\code{call}:}{Object of class \code{"language"} an unevaluated function call. }
    \item{\code{converged}:}{Object of class \code{"Ulogical"} a logical character indicates whether the iterations converged. }
    \item{\code{loadings}:}{Object of class \code{"matrix"} the matrix of variable loadings. }
    \item{\code{communality}:}{Object of class \code{"Uvector"} the communality. }
    \item{\code{uniquenesses}:}{Object of class \code{"vector"} the uniquenesses computed. }
	\item{\code{cor}:}{Object of class \code{"Ulogical"} A logical value indicating whether the calculation should use the covariance matrix (\code{cor = FALSE}) or the correlation matrix (\code{cor = TRUE}). }
    \item{\code{covariance}:}{Object of class \code{"matrix"} The robust/classical covariance matrix. }
    \item{\code{correlation}:}{Object of class \code{"matrix"} The robust/classical correlation matrix. }
    \item{\code{usedMatrix}:}{Object of class \code{"matrix"} The used matrix (running matrix). It may be the covariance or correlation matrix according to the value of \code{cor}. }
	\item{\code{reducedCorrelation}:}{Object of class \code{"Umatrix"} The last reduced correlation matrix. reducedCorrelation is only calculated in factorScorePfa.R. }
    \item{\code{criteria}:}{Object of class \code{"Unumeric"}. The results of the optimization: the value of the negative log-likelihood and information on the iterations used. }
    \item{\code{factors}:}{Object of class \code{"numeric"} the number of factors. }
    \item{\code{dof}:}{Object of class \code{"Unumeric"}. The number of degrees of freedom of the factor analysis model. }
    \item{\code{method}:}{Object of class \code{"character"}. The method: one of "mle", "pca", and "pfa". }
    \item{\code{scores}:}{Object of class \code{"Umatrix"}. If requested, a matrix of scores. }
    \item{\code{scoresMethod}:}{Object of class \code{"character"}. The scores method: one of "none", "regression", and "Bartlett". }
    \item{\code{scoringCoef}:}{Object of class \code{"Umatrix"} the matrix of scoring coefficients. }
    \item{\code{meanF}:}{Object of class \code{"Uvector"} the column means of scores. }
    \item{\code{corF}:}{Object of class \code{"Umatrix"} the correlation matrix of the scores. }
    \item{\code{STATISTIC}:}{Object of class \code{"Unumeric"}. The significance-test statistic, if it can be computed. }
    \item{\code{PVAL}:}{Object of class \code{"Unumeric"}. The significance-test P value, if it can be computed. }
    \item{\code{n.obs}:}{Object of class \code{"numeric"}. The number of observations. }
    \item{\code{center}:}{Object of class \code{"Uvector"}. The center of the data. }
    \item{\code{eigenvalues}:}{Object of class \code{"vector"} the eigenvalues. }
    \item{\code{cov.control}:}{Object of class \code{"UCovControl"}. Record the cov control method. }
  }
}

\section{Methods}{
  \describe{
    \item{getCenter}{\code{signature(obj = "Fa")}: center of the data }
    \item{getEigenvalues}{\code{signature(obj = "Fa")}: the eigenvalues of the covariance/correlation matrix }
    \item{getFa}{\code{signature(obj = "Fa")}: returns an S3 list of class \code{fa} for compatibility with the function factanal(). Thus the standard  screeplot() can be used. }
    \item{getLoadings}{\code{signature(obj = "Fa")}: returns the matrix loadings }
    \item{getQuan}{\code{signature(obj = "Fa")}: returns the number of observations used in the computation, i.e., n.obs }
    \item{getScores}{\code{signature(obj = "Fa")}: if requested, a matrix of scores. }
    \item{getSdev}{\code{signature(obj = "Fa")}: returns the standard deviations of the factor analysis, i.e., the square roots of the eigenvalues of the covariance/correlation matrix }
    \item{plot}{\code{signature(x = "Fa", y = "missing")}: produces a scatterplot of the factor scores (if which = "factorScore") or shows the eigenvalues plot (if which = "screeplot") }    
    \item{predict}{\code{signature(object = "Fa")}: calculates prediction using the results in object. The newdata argument is an optional data frame or matrix in which to look for variables with which to predict. If newdata is omitted, the scores are used. }
    \item{print}{\code{signature(x = "Fa")}: prints the results. obj = print(obj) = show(obj) }
    \item{show}{\code{signature(object = "Fa")}: prints the results. obj = print(obj) = show(obj) }
    \item{summary}{\code{signature(object = "Fa")}: produce result summaries of an object of class "Fa". }
	 }
}

\references{

Bartlett, M. S. (1937) The statistical conception of mental factors. \emph{British Journal of Psychology}, \bold{28}, 97--104. 

Bartlett, M. S. (1938) Methods of estimating mental factors. \emph{Nature}, \bold{141}, 609--610. 

Joreskog, K. G. (1963) \emph{Statistical Estimation in Factor Analysis}. Almqvist and Wicksell. 

Lawley, D. N. and Maxwell, A. E. (1971) \emph{Factor Analysis as a Statistical Method}. Second edition. Butterworths. 

Thomson, G. H. (1951) \emph{The Factorial Analysis of Human Ability}. London University Press. 

Zhang, Y. Y. (2013), An Object Oriented Solution for Robust Factor Analysis.

}

\author{ Ying-Ying Zhang (Robert) \email{robertzhangyying@qq.com} }


\seealso{
\code{\link{FaClassic-class}}, \code{\link{FaCov-class}}, \code{\link{FaRobust-class}}, \code{\link{Fa-class}}
}

\examples{
showClass("Fa")
}

\keyword{classes}
