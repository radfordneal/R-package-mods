\name{compute_cov_cor}
\alias{compute_cov_cor}

\title{
Compute the Robust Covariance and Correlation Matrix of A Numeric Matrix
}
\description{
Compute the robust covariance and correlation matrix of a numeric matrix. The function is used to check whether S_r != S_r_tilda and R_r == R_r_tilda?
}
\usage{
compute_cov_cor(x, control)
}

\arguments{

  \item{x}{
A numeric matrix or an object that can be coerced to a numeric matrix.
}
  \item{control}{
A control object (S4) for one of the available control classes, e.g. \code{CovControlMcd-class}, \code{CovControlOgk-class}, \code{CovControlSest-class}, etc., containing estimation options. The class of this object defines which estimator will be used. Alternatively a character string can be specified which names the estimator - one of auto, sde, mcd, ogk, m, mve, sfast, surreal, bisquare, rocke. If "auto" is specified or the argument is missing, the function will select the estimator.
}
}

%% \details{}

\value{
A list with the following components:

\item{S_r }{The robust covariance matrix of cov_x.}

\item{S_r_tilda }{The robust covariance matrix of cov_scale_x.}

\item{R_r }{The robust correlation matrix of cov_x.}

\item{R_r_tilda }{The robust correlation matrix of cov_scale_x.}

cov_x = CovRobust(x = x, control = control)
cov_scale_x = CovRobust(x = scale(x), control = control)

}

\references{
Zhang, Y. Y. (2013), An Object Oriented Solution for Robust Factor Analysis.
}

\author{ Ying-Ying Zhang (Robert) \email{robertzhangyying@qq.com} }

%% \seealso{}

\examples{
data("hbk")
hbk.x = hbk[,1:3]

compute_cov_cor(x = hbk.x, control = "mcd")

}

\keyword{robust}
