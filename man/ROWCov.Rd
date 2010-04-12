\name{ROWCov}
\Rdversion{1.1}
\alias{ROWCov}
\title{
Realized Outlyingness Weighted Covariance}

\description{
Function returns the Realized Outlyingness Weighted Covariance, defined in Boudt et al. (2008).

Let \eqn{r_{t,i}}, for \eqn{i=1,...,M} be a sample
of \eqn{M} high-frequency \eqn{(N x 1)} return vectors and \eqn{d_{t,i}}
their outlyingness given by the squared Mahalanobis distance between
the return vector and zero in terms of the reweighted MCD covariance
estimate based on these returns.

Then, the ROWCov is given by
\deqn{
\mbox{ROWCov}_{t}=c_{w}\frac{\sum_{i=1}^{M}w(d_{t,i})r_{t,i}r'_{t,i}}{\frac{1}{M}\sum_{i=1}^{M}w(d_{t,i})},
}
where \eqn{w(\cdot)} is a hard rejection weight function and \eqn{c_{w}} the corresponding 
correction factor, assuming the returns follow a normal distribution, 
as described in Boudt et al. (2008).
}

\usage{
ROWCov(data, seasadjR = NULL, wfunction = "HR" , alphaMCD = 0.5, alpha = 0.001)
}

\arguments{
  \item{data}{ a \eqn{(M x N)} matrix/zoo/xts object containing the \eqn{N}
return series over period \eqn{t}, with \eqn{M} observations during \eqn{t}.}
  \item{seasadjR}{ a \eqn{(M x N)} matrix/zoo/xts object containing 
the seasonaly adjusted returns. This is an optional argument.}
   \item{wfunction}{ determines whether 
a zero-one weight function (one if no jump is detected based on \eqn{d_{t,i}} and 0 otherwise)
or 
Soft Rejection ("SR") weight function is to be used.
By default a zero-one weight function (wfunction = "HR") is used.}
   \item{alphaMCD}{ a numeric parameter, controlling the size of 
the subsets over which the determinant is minimized. 
Allowed values are between 0.5 and 1 and 
the default is 0.75. See Boudt et al. (2008) or the \code{covMcd} function in the
robustbase package.}
   \item{alpha}{ is a parameter between 0 en 0.5, 
that determines the rejection threshold value 
(see Boudt et al. (2008) for details).}
}

\section{Details}{}

\value{
an \eqn{N x N} matrix
}

\references{
Boudt, K., C. Croux, and S. Laurent (2008). Outlyingness weighted quadratic
covariation. Mimeo.
}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd