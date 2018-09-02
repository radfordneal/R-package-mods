%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  FileMatrix.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{rowSums.FileMatrix}
\alias{rowSums.FileMatrix}
\alias{FileMatrix.rowSums}
\alias{rowSums,FileMatrix-method}

\title{Calculates the sum for each row}

\description{
 Calculates the sum for each row.
}

\usage{
\method{rowSums}{FileMatrix}(x, na.rm=FALSE, doCount=FALSE, rows=NULL, columns=NULL, ...)
}

\arguments{
  \item{na.rm}{If \code{\link[base:logical]{TRUE}}, \code{\link[base]{NA}} values are excluded.}
  \item{doCount}{If \code{\link[base:logical]{TRUE}}, the number of included values are counted
     and returned as an attribute.}
  \item{rows}{An \code{\link[base]{integer}} \code{\link[base]{vector}} of rows for which the sum should
     be calculated.  If \code{\link[base]{NULL}}, all rows are considered.}
  \item{columns}{An \code{\link[base]{integer}} \code{\link[base]{vector}} of columns for which the sum should
     be calculated.  If \code{\link[base]{NULL}}, all columns are considered.}
  \item{...}{Not used.}
}

\value{
 Returns a \code{\link[base]{numeric}} \code{\link[base]{vector}}.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[R.huge:rowMeans.FileMatrix]{*rowMeans}()}.
  For more information see \code{\link{FileMatrix}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
