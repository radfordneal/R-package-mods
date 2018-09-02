%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  FileMatrix.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{[.FileMatrix}
\alias{[.FileMatrix}
\alias{FileMatrix.[}
\alias{[,FileMatrix-method}

\title{Gets a subset of elements of a file matrix}

\description{
 Gets a subset of elements of a file matrix.
}

\usage{
\method{[}{FileMatrix}(this, i, j, drop=FALSE)
}

\arguments{
  \item{i}{A \code{\link[base]{numeric}} \code{\link[base]{vector}} or row indices.}
  \item{j}{A \code{\link[base]{numeric}} \code{\link[base]{vector}} or column indices.}
  \item{drop}{If \code{\link[base:logical]{TRUE}}, if only a single row or column is retrived,
    it is returned as a \code{\link[base]{vector}}.}
}

\value{
 Returns a \code{\link[base]{matrix}} (or possibly a \code{\link[base]{vector}}).
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[R.huge:000-LB-LT_-.FileMatrix]{*[<-}()}.
  For more information see \code{\link{FileMatrix}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
