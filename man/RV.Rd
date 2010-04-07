\name{RV}
\Rdversion{1.1}
\alias{RV}
\title{
Realized Volatility
}
\description{
Function returns the Realized Variance (RV).

Let \eqn{r_{t,i}} be a return (with \eqn{i=1,\ldots,M}) in period \eqn{t}.
Then the Realized Variance is given by
the sum of the squared intraday returns
\deqn{\mbox{RV}_{t}= \sum_{i=1}^{M}r_{t,i}^{2}}
}

\usage{
RV(returnseries);
}

\arguments{
  \item{returnseries}{ a vector/zoo/xts object containing all returns in period t for one asset }
}

\section{Details}{
}

\value{
numeric
}
\references{
}
\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd