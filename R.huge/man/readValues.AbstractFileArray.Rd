%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  AbstractFileArray.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{readValues.AbstractFileArray}
\alias{readValues.AbstractFileArray}
\alias{AbstractFileArray.readValues}
\alias{readValues,AbstractFileArray-method}

\title{Reads individual values in the file array}

\description{
 Reads individual values in the file array.
}

\usage{
\method{readValues}{AbstractFileArray}(this, indices=NULL, mode=getStorageMode(this), size=getBytesPerCell(this),
  offset=getDataOffset(this), order=FALSE, ..., .checkArgs=TRUE)
}

\arguments{
  \item{indices}{A \code{\link[base]{numeric}} \code{\link[base]{vector}} of indices.}
  \item{mode}{The storage mode to read.}
  \item{size}{The number of bytes each values allocates on file.}
  \item{offset}{The file offset to the first value on file.}
  \item{...}{Not used.}
  \item{.checkArgs}{If \code{\link[base:logical]{TRUE}}, arguments are validated, otherwise not.}
}

\value{
 Returns a \code{\link[base]{numeric}} \code{\link[base]{vector}}.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[R.huge:readContiguousValues.AbstractFileArray]{*readContiguousValues}()} and \code{\link{readValues}}().
  For more information see \code{\link{AbstractFileArray}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
