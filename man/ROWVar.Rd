\name{ROWVar}
\Rdversion{1.1}
\alias{ROWVar}
\title{
Realized Outlyingness Weighted Variance
}
\description{
Function returns the Realized Outlyingness Weighted Variance (ROWVar), defined in Boudt et al. (2008).

Let \eqn{r_{t,i}} be a return (with \eqn{i=1,\ldots,M}) in period \eqn{t} and \eqn{d_{t,i}} 
a measure for the local outlyingness
of that return, as defined in Boudt et al. (2008). The ROWVar is then given by

\deqn{
\mbox{ROWVar}_{t}=c_{w}\frac{\sum_{i=1}^{M}w(d_{t,i})r_{t,i}^{2}}{\frac{1}{M}\sum_{i=1}^{M}w(d_{t,i})}.
}

}

\usage{
ROWVar(data, seasadjR = NULL, wfunction = "HR" , alphaMCD = 0.5, alpha = 0.001)
}

\arguments{
  \item{data}{ a vector/zoo/xts object containing all returns in period t for one asset. }
  \item{seasadjR}{ seasadjR is a matrix/zoo/xts object containing the seasonaly adjusted
returns in period t for one asset. This is an optional argument.}
  \item{wfunction}{ determines whether 
a zero-one weight function (one if no jump is detected based on \eqn{d_{t,i}} and 0 otherwise)
or 
Soft Rejection ("SR") weight function is to be used. 
By default a zero-one weight function (wfunction = "HR") is used.}
  \item{alphaMCD}{
a numeric parameter, controlling the size of the subsets over
which the determinant is minimized. Allowed values are between 0.5 and
1 and the default is 0.75. See Boudt et al. (2008) or the \code{covMcd} function in the
robustbase package.
}
  \item{alpha}{a parameter between 0 en 0.5, that determines the rejection
threshold value (see Boudt et al. (2008) for details).}
}

\section{Details}{
}

\value{
numeric
}

\references{
Boudt, K., C. Croux, and S. Laurent (2008). Outlyingness weighted quadratic
covariation. Mimeo.
}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd