%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  AbstractFileArray.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{delete.AbstractFileArray}
\alias{delete.AbstractFileArray}
\alias{AbstractFileArray.delete}
\alias{delete,AbstractFileArray-method}

\title{Deletes the file array from the file system}

\description{
 Deletes the file array from the file system.
 If the file array is open, it is first closed.
}

\usage{
\method{delete}{AbstractFileArray}(this, ...)
}

\arguments{
  \item{...}{Not used.}
}

\value{
  Returns (invisibly) \code{\link[base:logical]{TRUE}} if the file was successfully deleted (or did
  not exist in the first place), otherwise \code{\link[base:logical]{FALSE}}.
}

\author{Henrik Bengtsson}

\seealso{
  For more information see \code{\link{AbstractFileArray}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
