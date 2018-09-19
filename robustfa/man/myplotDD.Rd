\name{myplotDD}
\alias{myplotDD}
\alias{plot}

\title{
Distance-Distance Plot
}
\description{
\code{"myplotDD"} is a revised version of \code{".myddplot"} in \code{"plot-utils.R"} in the package \code{"rrcov"}. In \code{"myplotDD"}, \code{id.n} and \code{ind} are printed out.
}
\usage{
myplotDD(x, cutoff, id.n)
}

\arguments{
  \item{x}{
An S4 object of class \code{"CovRobust"}.
}
  \item{cutoff}{
The \code{cutoff} value used. If missing, \code{cutoff <- sqrt(qchisq(0.975, p))} by default.
}
  \item{id.n}{
Number of observations to identify by a label. If not supplied, the number of observations with robust distance larger than \code{cutoff} is used.
}

}
\details{
Distance-Distance Plot: Plot the vector \code{y=rd} (robust distances) against \code{x=md} (mahalanobis distances). Identify by a label the \code{id.n} observations with largest \code{rd}. If \code{id.n} is not supplied, calculate it as the number of observations larger than \code{cutoff}. Use \code{cutoff} to draw a horisontal and a vertical line. Draw also a dotted line with a slope 1.

\code{"myplotDD(x)"} is equivalent to \code{"plot(x, which="dd")"}.
\code{which}: indicate what kind of plot. If \code{which = "dd"}, then a distance-distance Plot.

}

\value{
A distance-distance plot is shown. Return a list with components:

\item{cutoff }{The \code{cutoff} value used. If missing, \code{cutoff <- sqrt(qchisq(0.975, p))} by default.}

\item{id.n }{Number of observations to identify by a label. If not supplied, the number of observations with robust distance larger than \code{cutoff} is used.}

\item{sort.y }{A list containing the sorted values of y (the robust distance)}

\item{ind }{The indices of the largest \code{id.n} observations whose robust distances are larger than \code{cutoff}.}

}

\references{
Zhang, Y. Y. (2013), An Object Oriented Solution for Robust Factor Analysis.
}

\author{ Ying-Ying Zhang (Robert) \email{robertzhangyying@qq.com} }

\seealso{
\code{\link{plot}}, \code{\link{qchisq}}, \code{\link{CovClassic}}, \code{\link{getDistance}}
}

\examples{

data(stock611)
covMcd=CovRobust(x=scale(stock611[,3:12]), control="mcd"); covMcd

## "myplotDD" shows id.n and ind.
## Note: id.n and ind change each time due to covMcd changes each time!
## However, the ind of largest robust distances do not change.
result = myplotDD(x=covMcd); result

## "myplotDD" is equivalent to "plot(x=covMcd, which="dd")".
plot(x=covMcd, which="dd")

}

\keyword{robust}
