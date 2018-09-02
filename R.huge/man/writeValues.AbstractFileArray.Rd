%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  AbstractFileArray.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{writeValues.AbstractFileArray}
\alias{writeValues.AbstractFileArray}
\alias{AbstractFileArray.writeValues}
\alias{writeValues,AbstractFileArray-method}

\title{Writes values to a file array}

\description{
 Writes values to a file array.
}

\usage{
\method{writeValues}{AbstractFileArray}(this, indices=NULL, values, mode=getStorageMode(this),
  size=getBytesPerCell(this), offset=getDataOffset(this), order=FALSE, ...)
}

\arguments{
  \item{indices}{An \code{\link[base]{integer}} \code{\link[base]{vector}} of indices to be updated.
    If \code{\link[base]{NULL}}, all cells are considered.}
  \item{values}{A \code{\link[base]{numeric}} \code{\link[base]{vector}} of values to be assigned.}
  \item{mode}{The storage mode to be used.}
  \item{size}{A \code{\link[base]{integer}} specifying the number of bytes per cell.}
  \item{offset}{A \code{\link[base]{integer}} specifying the file offset of the data section.}
  \item{order}{If \code{\link[base:logical]{TRUE}}, the data is reordered before being written.}
  \item{...}{Additional arguments passed to \code{\link[R.huge:writeAllValues.AbstractFileArray]{*writeAllValues}()}.}
}

\value{
  Returns nothing.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[R.huge:writeAllValues.AbstractFileArray]{*writeAllValues}()}.
  For more information see \code{\link{AbstractFileArray}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
