\name{p_return}
\Rdversion{1.1}
\alias{p_return}
\title{
Compute log returns
}

\description{
Function returns an xts object 
with the log returns on Trade prices as xts object.

\deqn{
\mbox{log return on Trade prices}_t =  (\log(\mbox{PRICE}_{t})-\log(\mbox{PRICE}_{t-1})).
}
}

\usage{
p_return(data);
}

\arguments{
  \item{data}{ 
xts object, containing at least a column PRICE.)
}
}

\section{Details}{
Note: the first observation is set to zero.
}

\value{
an xts object containing the log returns.
}

\references{
}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd