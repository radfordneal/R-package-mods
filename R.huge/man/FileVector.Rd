%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  FileVector.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{FileVector}
\docType{class}
\alias{FileVector}

\alias{FileByteVector}
\alias{FileShortVector}
\alias{FileIntegerVector}
\alias{FileFloatVector}
\alias{FileDoubleVector}

\title{Class representing a persistent vector stored on file}

\description{
 Package:  R.huge \cr
\bold{Class FileVector}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.huge]{AbstractFileArray}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{FileVector}\cr

\bold{Directly known subclasses:}\cr
\link[R.huge]{FileByteVector}, \link[R.huge]{FileDoubleVector}, \link[R.huge]{FileFloatVector}, \link[R.huge]{FileIntegerVector}, \link[R.huge]{FileShortVector}\cr

public static class \bold{FileVector}\cr
extends \link[R.huge]{AbstractFileArray}\cr


}

\usage{
FileVector(..., length=NULL, names=NULL)
}

\arguments{
 \item{...}{Arguments passed to \code{\link{AbstractFileArray}}.}
 \item{length}{The number of elements in the vector.}
 \item{names}{Optional element names.}
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{[} \tab  -\cr
 \tab \code{[<-} \tab  -\cr
 \tab \code{\link[R.huge:names.FileVector]{names}} \tab Gets the element names of a file vector.\cr
}


 \bold{Methods inherited from AbstractFileArray}:\cr
as.character, as.vector, clone, close, delete, dim, dimnames, finalize, flush, getBasename, getBytesPerCell, getCloneNumber, getComments, getDataOffset, getDimensionOrder, getExtension, getFileSize, getName, getPath, getPathname, getSizeOfComments, getSizeOfData, getStorageMode, isOpen, length, open, readAllValues, readContiguousValues, readHeader, readValues, setComments, writeAllValues, writeEmptyData, writeHeader, writeHeaderComments, writeValues

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, save



}

\details{
  The purpose of this class is to be able to work with large vectors
  in \R without being limited by the amount of memory available.
  Data is kept on the file system and elements are read and written
  whenever queried.

  For more details, \code{\link{AbstractFileArray}}.
}

\section{Supported data types}{
  The following subclasses implement support for various data types:
  \itemize{
   \item \code{FileByteVector} (1 byte per element),
   \item \code{FileShortVector} (2 bytes per element),
   \item \code{FileIntegerVector} (4 bytes per element),
   \item \code{FileFloatVector} (4 bytes per element), and
   \item \code{FileDoubleVector} (8 bytes per element).
  }
}

\examples{
library("R.utils")
verbose <- Arguments$getVerbose(TRUE)

pathname <- "example.Rvector"
if (isFile(pathname)) {
  file.remove(pathname)
  if (isFile(pathname)) {
    stop("File not deleted: ", pathname)
  }
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create a new file vector
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
verbose && enter(verbose, "Creating new vector")
# The length of the vector
length <- 1e6
X <- FileDoubleVector(pathname, length=length)
verbose && exit(verbose)
print(X)

verbose && enter(verbose, "Filling it with data")
idxs <- c(1:4,7:10)
x <- 1:length(idxs)
writeValues(X, indices=idxs, values=x)
verbose && exit(verbose)

verbose && enter(verbose, "Getting data again")
y <- readValues(X, indices=idxs)
verbose && exit(verbose)
stopifnot(all.equal(x,y))

verbose && enter(verbose, "Getting and setting data using [i,j]")
print(X[1:20])
i <- 13:15
X[i] <- 99:98
print(X[1:20])
verbose && exit(verbose)

delete(X)

rm(X)
}

\author{Henrik Bengtsson}


\keyword{classes}
