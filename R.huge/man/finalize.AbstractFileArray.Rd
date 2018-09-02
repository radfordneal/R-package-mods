%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  AbstractFileArray.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{finalize.AbstractFileArray}
\alias{finalize.AbstractFileArray}
\alias{AbstractFileArray.finalize}
\alias{finalize,AbstractFileArray-method}

\title{Internal: Clean up when file array is deallocated from memory}

\description{
 Internal: Clean up when file array is deallocated from memory.
}

\usage{
\method{finalize}{AbstractFileArray}(this, ...)
}

\arguments{
  \item{...}{Not used.}
}

\value{
 Returns nothing.
}

\details{
  The finalizer of a file array makes sure to close the file connection,
  if it is open.
}

\author{Henrik Bengtsson}

\seealso{
  For more information see \code{\link{AbstractFileArray}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
