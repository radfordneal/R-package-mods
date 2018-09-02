%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  AbstractFileArray.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{close.AbstractFileArray}
\alias{close.AbstractFileArray}
\alias{AbstractFileArray.close}
\alias{close,AbstractFileArray-method}

\title{Closes a connection to the data file of the file array}

\description{
 Closes a connection to the data file of the file array.
}

\usage{
\method{close}{AbstractFileArray}(con, ...)
}

\arguments{
  \item{...}{Not used.}
}

\value{
 Returns \code{\link[base:logical]{TRUE}} if the file was closed.  If the file is not opened,
 an exception is thrown.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[R.huge:isOpen.AbstractFileArray]{*isOpen}()}.
  \code{\link[R.huge:open.AbstractFileArray]{*open}()}.
  For more information see \code{\link{AbstractFileArray}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
