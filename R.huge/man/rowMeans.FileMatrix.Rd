%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  FileMatrix.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{rowMeans.FileMatrix}
\alias{rowMeans.FileMatrix}
\alias{FileMatrix.rowMeans}
\alias{rowMeans,FileMatrix-method}

\title{Calculates the means for each row}

\description{
 Calculates the means for each row.
}

\usage{
\method{rowMeans}{FileMatrix}(x, ..., doCount=FALSE)
}

\arguments{
  \item{...}{Arguments passed to \code{\link[R.huge:rowSums.FileMatrix]{*rowSums}()}.}
  \item{doCount}{If \code{\link[base:logical]{TRUE}}, the number of included values are counted
     and returned as an attribute.}
}

\value{
 Returns a \code{\link[base]{numeric}} \code{\link[base]{vector}}.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[R.huge:rowSums.FileMatrix]{*rowSums}()}.
  For more information see \code{\link{FileMatrix}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
