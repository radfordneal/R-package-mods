\name{summary-methods}
\docType{methods}
\alias{summary-methods}
\alias{summary,Fa-method}
\alias{summary}

\title{Summary an Object}

\description{
Produce result summaries of an object of class "Fa". 
}

\usage{
summary(object, ...)
} 

\arguments{

    \item{object}{an object of class \code{"Fa"} or of a class derived from \code{"Fa"}.
}

    \item{...}{
additional arguments, e.g., print.x=TRUE.
}

}

\section{Methods}{
\describe{

\item{\code{signature(object = "Fa")}}{
Summary an object of class "Fa".
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

faCovPcaRegMcd
summary(faCovPcaRegMcd)

}

\keyword{methods}

