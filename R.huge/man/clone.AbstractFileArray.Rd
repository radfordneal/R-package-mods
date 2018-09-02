%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  AbstractFileArray.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{clone.AbstractFileArray}
\alias{clone.AbstractFileArray}
\alias{AbstractFileArray.clone}
\alias{clone,AbstractFileArray-method}

\title{Clones a file array}

\description{
 Clones a file array including the file on the file system.
}

\usage{
\method{clone}{AbstractFileArray}(con, copyData=TRUE, ...)
}

\arguments{
  \item{copyData}{If \code{\link[base:logical]{TRUE}}, also the data file is copied, otherwise not.}
  \item{...}{Not used.}
}

\value{
  Returns the new \code{\link{AbstractFileArray}} object.
}

\author{Henrik Bengtsson}

\seealso{
  For more information see \code{\link{AbstractFileArray}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
