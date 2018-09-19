\name{myFaPrint}
\alias{myFaPrint}

\title{Show/Print/Display an Object}

\description{
Show/print/display an object, including the Call, Standard deviations, Loadings, and Rotated variables (if \code{print.x = TRUE}).
}

\usage{
myFaPrint(object, print.x=FALSE)
} 

\arguments{

    \item{object}{
an object of class \code{"Fa"} or of a class derived from \code{"Fa"}. 
}

    \item{print.x}{
Logical. If \code{print.x = TRUE}, then print the rotated variables (scores).
}

}

\value{
An invisible argument \code{object}.
}

\references{
Zhang, Y. Y. (2013), An Object Oriented Solution for Robust Factor Analysis.
}

\author{ Ying-Ying Zhang (Robert) \email{robertzhangyying@qq.com} }

\seealso{\code{\link{Fa-class}}}

\examples{

data("hbk")
hbk.x = hbk[,1:3] 

faCovPcaRegMcd = FaCov(x = hbk.x, factors = 2, method = "pca",
scoresMethod = "regression", cov.control = CovControlMcd())

## object=show(object)=print(object)=myFaPrint(object)
## faCovPcaRegMcd is an object of class "Fa"
faCovPcaRegMcd
show(faCovPcaRegMcd)
print(faCovPcaRegMcd)
myFaPrint(faCovPcaRegMcd)

}

\keyword{robust}
