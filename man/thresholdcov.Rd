\name{thresholdcov}
\Rdversion{1.1}
\alias{thresholdcov}
\title{
Threshold Covariance}

\description{
Function returns the treshold covariance matrix proposed in Gobbi and Mancini (2009).

Let \eqn{r_{t,i}} be an intraday \eqn{N x 1} return vector and \eqn{i=1,...,M}
the number of intraday returns.

Then, the \eqn{k,q}-th element of the threshold covariance matrix is defined as
\deqn{
\mbox{tresholdcov}[k,q]_{t} = \sum_{i=1}^{M} r_{(k)t,i} 1_{\{r_{(k)t,i}^2 \leq TR_{M}\}}  \ \ r_{(q)t,i} 1_{\{r_{(q)t,i}^2 \leq TR_{M}\}},
}

with the treshold value \eqn{TR_{M}} taken as suggested in Jacod and Todorov (2009).
}

\usage{
thresholdcov(data)
}

\arguments{
  \item{data}{ a \eqn{(M x N)} matrix/zoo/xts object containing the \eqn{N}
return series over period \eqn{t}, with \eqn{M} observations during \eqn{t}.}
}

\section{Details}{}

\value{
an \eqn{N x N} matrix
}

\references{
Barndorff-Nielsen, O. and N. Shephard (2004). Measuring the impact of
jumps in multivariate price processes using bipower covariation. Discussion
paper, Nuffield College, Oxford University.

Jacod, J. and V. Todorov (2009). Testing for common arrival of jumps in
discretely-observed multidimensional processes. Annals of Statistics 37,
1792-1838.}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd