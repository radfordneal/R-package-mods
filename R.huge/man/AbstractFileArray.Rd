%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  AbstractFileArray.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{AbstractFileArray}
\docType{class}
\alias{AbstractFileArray}


\title{Class representing a persistent array stored in a file}

\description{
 Package:  R.huge \cr
\bold{Class AbstractFileArray}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{AbstractFileArray}\cr

\bold{Directly known subclasses:}\cr
\link[R.huge]{FileByteMatrix}, \link[R.huge]{FileByteVector}, \link[R.huge]{FileDoubleMatrix}, \link[R.huge]{FileDoubleVector}, \link[R.huge]{FileFloatMatrix}, \link[R.huge]{FileFloatVector}, \link[R.huge]{FileIntegerMatrix}, \link[R.huge]{FileIntegerVector}, \link[R.huge]{FileMatrix}, \link[R.huge]{FileShortMatrix}, \link[R.huge]{FileShortVector}, \link[R.huge]{FileVector}\cr

public static class \bold{AbstractFileArray}\cr
extends \link[R.oo]{Object}\cr



 Note that this is an abstract class, i.e. it is not possible to create
 an object of this class but only from one of its subclasses.
 For a vector data type, see \code{\link{FileVector}}.
 For a matrix data type, see \code{\link{FileMatrix}}.
}

\usage{
AbstractFileArray(filename=NULL, path=NULL, storageMode=c("integer", "double"),
  bytesPerCell=1, dim=NULL, dimnames=NULL, dimOrder=NULL, comments=NULL,
  nbrOfFreeBytes=4096)
}

\arguments{
 \item{filename}{The name of the file storing the data.}
 \item{path}{An optional path where data should be stored.}
 \item{storageMode}{The storage \code{\link{mode}}() of the data elements.}
 \item{bytesPerCell}{The number of bytes each element (cell) takes up
     on the file system.  If \code{NULL}, it is inferred from the
     \code{storageMode} argument.}
 \item{dim}{A \code{\link[base]{numeric}} \code{\link[base]{vector}} specifying the dimensions of the array.}
 \item{dimnames}{An optional \code{\link[base]{list}} of dimension names.}
 \item{dimOrder}{The order of the dimensions.}
 \item{comments}{An optional \code{\link[base]{character}} string of arbitrary length.}
 \item{nbrOfFreeBytes}{The number of "spare" bytes after the comments
     before the data section begins.}
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[R.huge:as.character.AbstractFileArray]{as.character}} \tab Returns a short string describing the file array.\cr
 \tab \code{\link[R.huge:as.vector.AbstractFileArray]{as.vector}} \tab Returns the elements of a file array as an R vector.\cr
 \tab \code{\link[R.huge:clone.AbstractFileArray]{clone}} \tab Clones a file array.\cr
 \tab \code{\link[R.huge:close.AbstractFileArray]{close}} \tab Closes a connection to the data file of the file array.\cr
 \tab \code{\link[R.huge:delete.AbstractFileArray]{delete}} \tab Deletes the file array from the file system.\cr
 \tab \code{\link[R.huge:dim.AbstractFileArray]{dim}} \tab Gets the dimension of the file array.\cr
 \tab \code{\link[R.huge:dimnames.AbstractFileArray]{dimnames}} \tab Gets the dimension names of a file array.\cr
 \tab \code{\link[R.huge:finalize.AbstractFileArray]{finalize}} \tab Internal: Clean up when file array is deallocated from memory.\cr
 \tab \code{\link[R.huge:flush.AbstractFileArray]{flush}} \tab Internal: Flushes the write buffer.\cr
 \tab \code{\link[R.huge:getBasename.AbstractFileArray]{getBasename}} \tab Gets the basename (filename) of the data file.\cr
 \tab \code{\link[R.huge:getBytesPerCell.AbstractFileArray]{getBytesPerCell}} \tab Gets the number of bytes per element in a file array.\cr
 \tab \code{\link[R.huge:getCloneNumber.AbstractFileArray]{getCloneNumber}} \tab Gets the clone number of the file array.\cr
 \tab \code{\link[R.huge:getComments.AbstractFileArray]{getComments}} \tab Gets the comments for this file array.\cr
 \tab \code{\link[R.huge:getDataOffset.AbstractFileArray]{getDataOffset}} \tab Gets file position of the data section in a file array.\cr
 \tab \code{\link[R.huge:getDimensionOrder.AbstractFileArray]{getDimensionOrder}} \tab Gets the order of dimension.\cr
 \tab \code{\link[R.huge:getExtension.AbstractFileArray]{getExtension}} \tab Gets the filename extension of the file array.\cr
 \tab \code{\link[R.huge:getFileSize.AbstractFileArray]{getFileSize}} \tab Gets the size of the file array.\cr
 \tab \code{\link[R.huge:getName.AbstractFileArray]{getName}} \tab Gets the name of the file array.\cr
 \tab \code{\link[R.huge:getPath.AbstractFileArray]{getPath}} \tab Gets the path (directory) where the data file lives.\cr
 \tab \code{\link[R.huge:getPathname.AbstractFileArray]{getPathname}} \tab Gets the full pathname to the data file.\cr
 \tab \code{\link[R.huge:getSizeOfComments.AbstractFileArray]{getSizeOfComments}} \tab Gets the number of bytes the comments occupies.\cr
 \tab \code{\link[R.huge:getSizeOfData.AbstractFileArray]{getSizeOfData}} \tab Gets the size of the data section in bytes.\cr
 \tab \code{\link[R.huge:getStorageMode.AbstractFileArray]{getStorageMode}} \tab Gets the storage mode of the file array.\cr
 \tab \code{\link[R.huge:isOpen.AbstractFileArray]{isOpen}} \tab Checks whether the data file of the file array is open or not.\cr
 \tab \code{\link[R.huge:length.AbstractFileArray]{length}} \tab Gets the number of elements in a file array.\cr
 \tab \code{\link[R.huge:open.AbstractFileArray]{open}} \tab Opens a connection to the data file of the file array.\cr
 \tab \code{\link[R.huge:readAllValues.AbstractFileArray]{readAllValues}} \tab Reads all values in the file array.\cr
 \tab \code{\link[R.huge:readContiguousValues.AbstractFileArray]{readContiguousValues}} \tab Reads sets of contiguous values in the file array.\cr
 \tab \code{\link[R.huge:readHeader.AbstractFileArray]{readHeader}} \tab Read the header of a file array data file.\cr
 \tab \code{\link[R.huge:readValues.AbstractFileArray]{readValues}} \tab Reads individual values in the file array.\cr
 \tab \code{\link[R.huge:setComments.AbstractFileArray]{setComments}} \tab Sets the comments for this file array.\cr
 \tab \code{\link[R.huge:writeAllValues.AbstractFileArray]{writeAllValues}} \tab Writes all values to a file array.\cr
 \tab \code{\link[R.huge:writeEmptyData.AbstractFileArray]{writeEmptyData}} \tab Writes an empty data section to the data file of a file array.\cr
 \tab \code{\link[R.huge:writeHeader.AbstractFileArray]{writeHeader}} \tab Writes the header of a file array to file.\cr
 \tab \code{writeHeaderComments} \tab  -\cr
 \tab \code{\link[R.huge:writeValues.AbstractFileArray]{writeValues}} \tab Writes values to a file array.\cr
}


 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, save



}

\details{
  The purpose of this class is to be able to work with large arrays
  in \R without being limited by the amount of memory available.
  Data is kept on the file system and elements are read and written
  whenever queried.
}

\section{Maximum number of elements}{
  It is only the header that is kept in memory, not the data, and
  therefore the maximum length of a array that can be allocate, is limited
  by the amount of available space on the file system.
  Since element names (optional) are stored in the header,
  these may also be a limiting factor.
}

\section{Element names}{
  The element names are stored in the header and are currently read and
  written to file one by one.  This may slow down the performance
  substantially if the dimensions are large.  For optimal opening
  performance, avoid names.

  For now, do \emph{not} change names after file has been allocated.
}

\section{File format}{
  The file format consist of a header section and a data section.
  The header contains information about the file format, the length
  and element names of the array, as well as data type
  (storage \code{\link{mode}}()), the size of each element.
  The data section, which follows immediately after the header section,
  consists of all data elements with non-assigned elements being
  pre-allocated with zeros.

  For more details, see the source code.
}

\section{Limitations}{
  The size of the array in bytes is limited by the maximum file size
  of the file system.
  For instance, the maximum file size on a Windows FAT32 system is
  4GB (2GB?).  On Windows NTFS the limit is in practice ~16TB.
}

\author{Henrik Bengtsson}

\references{
 [1] New Technology File System (NTFS), Wikipedia, 2006
     \url{http://en.wikipedia.org/wiki/NTFS}.
}


\keyword{classes}
