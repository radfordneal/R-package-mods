\name{stock611}
\alias{stock611}
\docType{data}
\title{
The Stocks Data - Year 2001
}
\description{
This data set consists of 611 observations with 12 variables.
}
\usage{data(stock611)}
\format{
  A data frame with 611 observations on the following 12 variables.
  \describe{
    \item{\code{code}}{a numeric vector}
    \item{\code{name}}{a numeric vector: the Chinese stocks name is replaced by integer, it can be found by its code.}
    \item{\code{x1}}{a numeric vector: main business income (China Yuan)}
    \item{\code{x2}}{a numeric vector: main business profit (China Yuan)}
    \item{\code{x3}}{a numeric vector: total profit (China Yuan)}
    \item{\code{x4}}{a numeric vector: net profit (China Yuan)}
    \item{\code{x5}}{a numeric vector: earnings per share (EPS) (China Yuan)}
    \item{\code{x6}}{a numeric vector: net assets per share (China Yuan)}
    \item{\code{x7}}{a numeric vector: net return on assets (\%)}
    \item{\code{x8}}{a numeric vector: total return on assets (\%)}
    \item{\code{x9}}{a numeric vector: total assets (China Yuan)}
    \item{\code{x10}}{a numeric vector: equity}
  }
}
\details{
The data set is from Chinese stock market in the year 2001. It was used in Wang X. M. (2009) to illustrate the factor analysis methods.
}
\source{
Wang X. M. (2009) \emph{Applied Multivariate Analysis}. Third edition. ShangHai University of Finance & Economics Press. (This is a Chinese book)

Note: In Wang X. M.'s homepage, he provided a link to download materials related to his book (including the data set stock611): http://bb.shufe.edu.cn/bbcswebdav/institution/%E7%BB%9F%E8%AE%A1%E5%AD%A6%E7%B3%BB/teacherweb/1987000028/index.htm
}

%% \references{}

\examples{
data(stock611)
str(stock611)
plot(stock611)
}
\keyword{datasets}
