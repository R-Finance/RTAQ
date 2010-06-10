\name{p_return_abs}
\Rdversion{1.1}
\alias{p_return_abs}
\title{
Absolute log returns
}

\description{
Function returns an xts object 
with the absolute log return on Trade prices as xts object.

\deqn{
\mbox{absolute log return on Trade prices}_t =  |\log(\mbox{PRICE}_{t})-\log(\mbox{PRICE}_{t-1})|.
}

}

\usage{
p_return_abs(data)
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
an xts object containing the absolute log returns.
}

\references{
}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd