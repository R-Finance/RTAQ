\name{p_return_sqr}
\Rdversion{1.1}
\alias{p_return_sqr}
\title{
Squared log returns
}

\description{
Function returns an xts object 
with the squared log return on Trade prices as xts object.

\deqn{
\mbox{squared log return on Trade prices}_t =  (\log(\mbox{PRICE}_{t})-\log(\mbox{PRICE}_{t-1}))^2.
}

}

\usage{
p_return_sqr(data);
}

\arguments{
  \item{data}{ 
xts object, containing joined trades and quotes (e.g. using \code{\link{matchtq}})
}
}

\section{Details}{
Note: the first observation is set to zero.
}

\value{
an xts object containing the squared log returns.
}

\references{
}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd