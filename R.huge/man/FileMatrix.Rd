%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  FileMatrix.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{FileMatrix}
\docType{class}
\alias{FileMatrix}

\alias{FileByteMatrix}
\alias{FileShortMatrix}
\alias{FileIntegerMatrix}
\alias{FileFloatMatrix}
\alias{FileDoubleMatrix}

\title{Class representing a persistent matrix stored in a file}

\description{
 Package:  R.huge \cr
\bold{Class FileMatrix}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.huge]{AbstractFileArray}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{FileMatrix}\cr

\bold{Directly known subclasses:}\cr
\link[R.huge]{FileByteMatrix}, \link[R.huge]{FileDoubleMatrix}, \link[R.huge]{FileFloatMatrix}, \link[R.huge]{FileIntegerMatrix}, \link[R.huge]{FileShortMatrix}\cr

public static class \bold{FileMatrix}\cr
extends \link[R.huge]{AbstractFileArray}\cr


}

\usage{
FileMatrix(..., nrow=NULL, ncol=NULL, rownames=NULL, colnames=NULL, byrow=FALSE)
}

\arguments{
 \item{...}{Arguments passed to \code{\link{AbstractFileArray}}.}
 \item{nrow, ncol}{The number of rows and columns of the matrix.}
 \item{rownames, colnames}{Optional row and column names.}
 \item{byrow}{If \code{\link[base:logical]{TRUE}}, data are stored row by row, otherwise column
     by column.}
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{[} \tab  -\cr
 \tab \code{[<-} \tab  -\cr
 \tab \code{\link[R.huge:as.character.FileMatrix]{as.character}} \tab Returns a short string describing the file matrix.\cr
 \tab \code{\link[R.huge:as.matrix.FileMatrix]{as.matrix}} \tab Returns the elements of a file matrix as an R matrix.\cr
 \tab \code{\link[R.huge:colnames.FileMatrix]{colnames}} \tab Gets the column names of a file matrix.\cr
 \tab \code{\link[R.huge:getByRow.FileMatrix]{getByRow}} \tab Checks if elements are stored row by row or not.\cr
 \tab \code{getColumnOffset} \tab  -\cr
 \tab \code{getMatrixIndicies} \tab  -\cr
 \tab \code{getOffset} \tab  -\cr
 \tab \code{getRowOffset} \tab  -\cr
 \tab \code{\link[R.huge:ncol.FileMatrix]{ncol}} \tab Gets the number of columns of the matrix.\cr
 \tab \code{\link[R.huge:nrow.FileMatrix]{nrow}} \tab Gets the number of rows of the matrix.\cr
 \tab \code{readFullMatrix} \tab  -\cr
 \tab \code{readValues} \tab  -\cr
 \tab \code{\link[R.huge:rowMeans.FileMatrix]{rowMeans}} \tab Calculates the means for each row.\cr
 \tab \code{\link[R.huge:rowSums.FileMatrix]{rowSums}} \tab Calculates the sum for each row.\cr
 \tab \code{\link[R.huge:rownames.FileMatrix]{rownames}} \tab Gets the row names of a file matrix.\cr
 \tab \code{writeValues} \tab  -\cr
}


 \bold{Methods inherited from AbstractFileArray}:\cr
as.character, as.vector, clone, close, delete, dim, dimnames, finalize, flush, getBasename, getBytesPerCell, getCloneNumber, getComments, getDataOffset, getDimensionOrder, getExtension, getFileSize, getName, getPath, getPathname, getSizeOfComments, getSizeOfData, getStorageMode, isOpen, length, open, readAllValues, readContiguousValues, readHeader, readValues, setComments, writeAllValues, writeEmptyData, writeHeader, writeHeaderComments, writeValues

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, save



}

\details{
  The purpose of this class is to be able to work with large matrices
  in \R without being limited by the amount of memory available.
  Matrices are kept on the file system and elements are read and written
  whenever queried.  The purpose of the class is \emph{not} to provide
  methods for full matrix operations, but instead to be able to work
  with subsets of the matrix at each time.

  For more details, \code{\link{AbstractFileArray}}.
}

\section{Column by column or row by row?}{
  If the matrix elements are to be accessed more often along rows,
  store data row by row, otherwise column by column.
}

\section{Supported data types}{
  The following subclasses implement support for various data types:
  \itemize{
   \item \code{FileByteMatrix} (1 byte per element),
   \item \code{FileShortMatrix} (2 bytes per element),
   \item \code{FileIntegerMatrix} (4 bytes per element),
   \item \code{FileFloatMatrix} (4 bytes per element), and
   \item \code{FileDoubleMatrix} (8 bytes per element).
  }
}

\examples{
library("R.utils")
verbose <- Arguments$getVerbose(TRUE)

pathname <- "example.Rmatrix"
if (isFile(pathname)) {
  file.remove(pathname)
  if (isFile(pathname)) {
    stop("File not deleted: ", pathname)
  }
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create a new file matrix
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
verbose && enter(verbose, "Creating new matrix")
# The dimensions of the matrix
nrow <- 20
ncol <- 5
X <- FileByteMatrix(pathname, nrow=nrow, ncol=ncol, byrow=TRUE)
verbose && exit(verbose)

verbose && enter(verbose, "Filling it with data")
rows <- c(1:4,7:10)
cols <- c(1)
x <- 1:length(rows)
writeValues(X, rows=rows, cols=cols, values=x)
verbose && exit(verbose)

verbose && enter(verbose, "Getting data again")
y <- readValues(X, rows=rows, cols=cols)
verbose && exit(verbose)
stopifnot(all.equal(x,y))

verbose && enter(verbose, "Setting data using [i,j]")
i <- c(20:18, 13:15)
j <- c(3:2, 4:5)
n <- length(i) * length(j)
values <- 1:n
X[i,j] <- values
verbose && enter(verbose, "Validating")
print(X)
print(X[])
print(X[i,j])
stopifnot(all.equal(as.vector(X[i,j]), values))
verbose && exit(verbose)
verbose && exit(verbose)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Open an already existing file matrix
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
verbose && enter(verbose, "Getting existing matrix")
Y <- FileByteMatrix(pathname)
verbose && exit(verbose)

print(Y[])
Y[5,1] <- 55
print(Y[])
print(X[])  # Note, X and Y refers to the same instance


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Clone a matrix
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Z <- clone(X)
Z[5,1] <- 66
print(Z[])
print(Y[])

# Remove clone again
delete(Z)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Close all matrices
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
close(X)
close(Y)

# Remove original matrix too
delete(X)
}

\author{Henrik Bengtsson}


\keyword{classes}
