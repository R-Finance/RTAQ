\name{RCor}
\Rdversion{1.1}
\alias{RCor}
\title{
Realized Correlation}

\description{
Function returns the realized correlation matrix.

Let \eqn{r_{t,i}} be an intraday \eqn{N x 1} return vector and \eqn{i=1,...,M}
the number of intraday returns.

Then, the \eqn{(k,q)}-th element of the 
Realized correlation matrix over period \eqn{t} is defined as
\deqn{
\mbox{RCor}[k,q]_t = \frac{\sum_{i=1}^{n}r_{(k)t,i}*r_{(q)t,i}}{\sqrt{\sum_{i=1}^{n}r^2_{(k)t,i}\sum_{i=1}^{n}r^2_{(q)t,i}}}
}
where \eqn{r_{(k)t,i}} is the \eqn{k}-th component of the return vector \eqn{r_{t,i}} 
and \eqn{n} the number of observations per day.
}

\usage{
RCor(data)
}

\arguments{
  \item{data}{ a \eqn{(M x N)} matrix/zoo/xts object containing the \eqn{N}
return series over period \eqn{t}, with \eqn{M} observations during \eqn{t}.}
}

\section{Details}{}

\value{
an \eqn{N x N} matrix
}

\references{}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd