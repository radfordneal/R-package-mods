\name{plot-methods}
\docType{methods}
\alias{plot-methods}
\alias{plot,Fa,missing-method}
\alias{plot,Fa-method}

\title{Plot an object of class "Fa"}

\description{
Plot an object of class "Fa".  If which = "factorScore", then a scatterplot of the factor scores is produced; if which = "screeplot", shows the eigenvalues and is helpful to select the number of factors.
}

\usage{
\S4method{plot}{Fa}(x, which=c("factorScore", "screeplot"), choices=1:2)
} 

\arguments{

    \item{x}{an object of class \code{"Fa"} or of a class derived from \code{"Fa"}}

    \item{which}{indicate what kind of plot. If \code{which = "factorScore"}, then a scatterplot of the factor scores is produced; if \code{which = "screeplot"}, shows the eigenvalues and is helpful to select the number of factors.
}

    \item{choices}{an integer vector indicate which columns of the factor scores to plot}

}

\details{
The feasible usages are:
\code{plot(x, which="factorScore", choices=1:2)}
\code{plot(x, which="screeplot")}
}

\section{Methods}{
\describe{

\item{\code{signature(x = "Fa", y = "missing")}}{
generic functions - see \code{plot}
}

}}

\references{
Zhang, Y. Y. (2013), An Object Oriented Solution for Robust Factor Analysis.
}

\author{ Ying-Ying Zhang (Robert) \email{robertzhangyying@qq.com} }

\examples{

data("hbk")
hbk.x = hbk[,1:3] 

faClassicPcaReg = FaClassic(x = hbk.x, factors = 2, method = "pca",
scoresMethod = "regression"); faClassicPcaReg
summary(faClassicPcaReg)

plot(faClassicPcaReg, which = "factorScore", choices = 1:2)
plot(faClassicPcaReg, which = "screeplot")

}

\keyword{methods}
