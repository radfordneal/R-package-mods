%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  999.package.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{R.huge-package}
\alias{R.huge-package}
\alias{R.huge}
\docType{package}

\title{Package R.huge}


\description{
  \emph{This package has been deprecated. Do not start building new
  projects based on it.}

  DEPRECATED. Do not start building new projects based on this package. Cross-platform alternatives are the following packages: bigmemory (CRAN), ff (CRAN), BufferedMatrix (Bioconductor).  The main usage of it was inside the aroma.affymetrix package. (The package currently provides a class representing a matrix where the actual data is stored in a binary format on the local file system.  This way the size limit of the data is set by the file system and not the memory.)
}

\section{Requirements}{
  This package requires the following CRAN packages:
  \pkg{R.methodsS3}, \pkg{R.oo} and \pkg{R.utils}.
}

\section{Installation and updates}{

  To install this package, use \code{install.packages("R.huge")}.
}

\section{To get started}{
  To get started, see:
  \enumerate{
    \item \code{\link{FileVector}}.
    \item \code{\link{FileMatrix}}.
  }
}

\section{How to cite this package}{
  Please cite [1] below.
}

\author{Henrik Bengtsson}

\section{License}{
  The releases of this package is licensed under
  LGPL version 2.1 or newer.

  The development code of the packages is under a private licence
  (where applicable) and patches sent to the author fall under the
  latter license, but will be, if incorporated, released under the
  "release" license above.
}

\section{References}{
[1] H. Bengtsson, \emph{The R.oo package - Object-Oriented Programming with References Using Standard R Code}, In Kurt Hornik, Friedrich Leisch and Achim Zeileis, editors, Proceedings of the 3rd International Workshop on Distributed Statistical Computing (DSC 2003), March 20-22, Vienna, Austria. \url{http://www.ci.tuwien.ac.at/Conferences/DSC-2003/Proceedings/}
 \cr
}
\keyword{package}
