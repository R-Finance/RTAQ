\name{RCov}
\Rdversion{1.1}
\alias{RCov}
\title{
Realized Covariance
}

\description{
Function returns the Realized Covariation (RCov).

Let \eqn{r_{t,i}} be an intraday \eqn{N x 1} return vector and \eqn{i=1,...,M} 
the number of intraday returns.

Then, the RCov is given by
\deqn{
\mbox{RCov}_{t}=\sum_{i=1}^{M}r_{t,i}r'_{t,i}.
}
}

\usage{
RCov(data)
}

\arguments{
  \item{data}{ a \eqn{(M x N)} matrix/zoo/xts object containing the \eqn{N}
return series over period \eqn{t}, with \eqn{M} observations during \eqn{t}.}
}

\section{Details}{
}

\value{
an \eqn{N x N} matrix
}

\references{
}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd