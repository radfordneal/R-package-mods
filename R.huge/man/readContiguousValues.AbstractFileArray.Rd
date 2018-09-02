%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  AbstractFileArray.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{readContiguousValues.AbstractFileArray}
\alias{readContiguousValues.AbstractFileArray}
\alias{AbstractFileArray.readContiguousValues}
\alias{readContiguousValues,AbstractFileArray-method}

\title{Reads sets of contiguous values in the file array}

\description{
 Reads sets of contiguous values in the file array.
 A set of \emph{contiguous} values are values that are connecting without
 a break.  It is much faster to read contiguous sequences at once than read
 each value separately.
}

\usage{
\method{readContiguousValues}{AbstractFileArray}(this, indices, lengths=1, mode=getStorageMode(this),
  size=getBytesPerCell(this), offset=getDataOffset(this), ..., .checkArgs=TRUE)
}

\arguments{
  \item{indices}{A \code{\link[base]{numeric}} \code{\link[base]{vector}} of start (first) indices in each of
    the contiguous sets.}
  \item{length}{A \code{\link[base]{numeric}} \code{\link[base]{vector}} specifying the length of each of the
    contiguous sets.}
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
  \code{\link[R.huge:readAllValues.AbstractFileArray]{*readAllValues}()} and \code{\link{readValues}}().
  For more information see \code{\link{AbstractFileArray}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
