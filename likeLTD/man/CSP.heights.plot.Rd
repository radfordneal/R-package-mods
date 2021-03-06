\name{CSP.heights.plot} 
\alias{CSP.heights.plot} 
\title{likeLTD::CSP.heights.plot} 
\usage{CSP.heights.plot(csp, refs, dbFile = NULL, kit = NULL, 
			outputFile = NULL, toPlot = NULL, 
			detectThresh = NULL, uncThresh = 0.05, 
			stutterThresh = 0.15, doStutter = FALSE,
			replicate = 1,...)}
\arguments{
  \item{csp}{CSP as returned from \code{read.peaks.profile}.}
  \item{refs}{Reference profiles as returned by \code{read.known.profiles}.}
  \item{dbFile}{Path to allele database file. If \code{NULL}, \code{kit} is used
	instead.}
  \item{kit}{Choice of an allele database that is included wiht likeLTD. If both 
	\code{dbFile} and \code{kit} are NULL, the DNA17 database is used.}
  \item{outputFile}{Path to output file. If \code{NULL}, the plot is output to a plot
	window instead.}
  \item{toPlot}{An integer vector specifying which loci to plot. If \code{NULL}, all loci
	are plotted.}
  \item{detectThresh}{Detection threshold used for electrophoresis analysis. If \code{NULL},
	this is not plotted.}
  \item{uncThresh}{Threshold for suggesting that a peak is uncertain.}
  \item{stutterThresh}{Threshold for suggesting a peak is allelic.}
  \item{doStutter}{Logical. Specifies whether to suggest peaks as non-allelic/uncertain/allelic.}
  \item{replicate}{Which replicate to plot.}
  \item{...}{Extra parameters to pass to \code{plot}.}
}
\value{
Pdf file or plot window.
} 
\description{
Plot one replicate from a peak heights CSP.
}
\details{
  If \code{refFile} is specified, the plotted peaks are coloured according to which 
  reference individuals possess that allele. If \code{detectThresh} is specified, a 
  horizontal line is plotted indicating the threshold. If \code{doStutter=TRUE}, the 
  labels for each peak are coloured by a crude estimation of which peaks are allelic, 
  uncertain, or non-allelic.
}
\seealso{allele.report.peaks}
\examples{
\dontrun{
# datapath to example files
datapath = file.path(system.file("extdata", package="likeLTD"),"laboratory")

# File paths and case name for allele report
admin = pack.admin.input.peaks(
            peaksFile = file.path(datapath, 'laboratory-CSP.csv'),
            refFile = file.path(datapath, 'laboratory-reference.csv'),
            caseName = "Laboratory",
            detectionThresh = 20
             )

# plot CSP heights
CSP.heights.plot(csp=read.peaks.profile(admin$peaksFile),
		ref=read.known.profiles(admin$refFile))

# to plot just the first four loci
CSP.heights.plot(csp=read.peaks.profile(admin$peaksFile),
		ref=read.known.profiles(admin$refFile),
		toplot=1:4)

# to estimate which peaks are non-allelic
CSP.heights.plot(csp=read.peaks.profile(admin$peaksFile),
		ref=read.known.profiles(admin$refFile),
		doStutter=TRUE)

# to display detection threshold
CSP.heights.plot(csp=read.peaks.profile(admin$peaksFile),
		ref=read.known.profiles(admin$refFile),
		detectThresh=20)
}
}


