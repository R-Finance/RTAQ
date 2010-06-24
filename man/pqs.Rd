\name{pqs}
\Rdversion{1.1}
\alias{pqs}
\title{
Proportional quoted spread
}

\description{
Function returns an xts object with the quoted spread, defined as

\deqn{
\mbox{proportional quoted spread}_t =  \frac{\mbox{quoted spread}_t}{\frac{\mbox{OFR}_{t}+\mbox{BID}_{t}}{2}}
}
(Venkataraman, 2001).
Note that the input of this function consists of the matched trades and
quotes, so this is where the time indication refers to (and thus not to the
registered quote timestamp).
}

\usage{
pqs(data)
}

\arguments{
  \item{data}{ 
xts object, containing joined trades and quotes (e.g. using \code{\link{matchtq}})
}
}

\section{Details}{
}

\value{
an xts object containing the proportional quoted spread.
}

\references{
Venkataraman, K. (2001). Automated versus floor trading: 
An analysis of execution costs on the paris and new york exchanges. 
The Journal of Finance, 56, 
1445-1485.
}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd