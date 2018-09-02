%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  AbstractFileArray.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{writeAllValues.AbstractFileArray}
\alias{writeAllValues.AbstractFileArray}
\alias{AbstractFileArray.writeAllValues}
\alias{writeAllValues,AbstractFileArray-method}

\title{Writes all values to a file array}

\description{
 Writes all values to a file array.
}

\usage{
\method{writeAllValues}{AbstractFileArray}(this, values, mode=getStorageMode(this), size=getBytesPerCell(this),
  offset=getDataOffset(this), ...)
}

\arguments{
  \item{values}{A \code{\link[base]{numeric}} \code{\link[base]{vector}} of values to be assigned.}
  \item{mode}{The storage mode to be used.}
  \item{size}{A \code{\link[base]{integer}} specifying the number of bytes per cell.}
  \item{offset}{A \code{\link[base]{integer}} specifying the file offset of the data section.}
  \item{...}{Not used.}
}

\value{
  Returns nothing.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[R.huge:writeValues.AbstractFileArray]{*writeValues}()}.
  For more information see \code{\link{AbstractFileArray}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
