\name{detail}
\alias{detail}

\title{
Show Details of an Object
}
\description{
Show details of an object.
}
\usage{
detail(x)
}

\arguments{
  \item{x}{
Any \code{R} object to be tested.
}
}
%% \details{}
\value{
A list with components:

\item{x }{The argument \code{x}.}

\item{isS4 }{Logical, indicates whether \code{x} is an S4 object.}

\item{isObject }{Logical, indicates whether \code{x} is an object, i.e., with a class attribute.}

\item{class }{The class of \code{x}.}

\item{attributes }{The attributes of \code{x}. Usually \code{result$attributes} is also a list.}

}

\references{
Zhang, Y. Y. (2013), An Object Oriented Solution for Robust Factor Analysis.
}

\author{ Ying-Ying Zhang (Robert) \email{robertzhangyying@qq.com} }

\seealso{
\code{\link{isS4}}, \code{\link{is.object}}, \code{\link{class}}, \code{\link{attributes}}
}

\examples{
data(stock611)
detail(stock611)

facovRegOgk=FaCov(x=scale(stock611[,3:12]), factors=3, cov.control = CovControlOgk(), 
scoresMethod = "regression"); facovRegOgk
detail(facovRegOgk)

}

\keyword{robust}

