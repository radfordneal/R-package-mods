\name{SummaryFa-class}
\Rdversion{1.1}
\docType{class}
\alias{SummaryFa-class}

\title{Class \code{"SummaryFa"}}
\description{
Summary of \code{"Fa"} objects. The \code{"Fa"} object plus some additional summary information.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SummaryFa", ...)}. But most often by invoking 'summary' on an "Fa" object. They contain values meant for printing by 'show'.
}
\section{Slots}{
  \describe{
    \item{\code{faobj}:}{Object of class \code{"Fa"} }
    \item{\code{importance}:}{Object of class \code{"matrix"}. Matrix with additional information: importance of components. }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "SummaryFa")}: display the object }
	 }
}

\references{
Zhang, Y. Y. (2013), An Object Oriented Solution for Robust Factor Analysis.
}

\author{ Ying-Ying Zhang (Robert) \email{robertzhangyying@qq.com} }

\seealso{
\code{\link{Fa-class}}
}
\examples{
showClass("SummaryFa")
}
\keyword{classes}
