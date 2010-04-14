\name{prop_price_impact}
\Rdversion{1.1}
\alias{prop_price_impact}
\title{
Proportional price impact
}

\description{
Function returns an xts object with the proportional price impact, defined as

\deqn{
\mbox{proportional price impact}_t =  \frac{100*\frac{(\mbox{effective spread}_t - \mbox{realized spread}_t)}{2}}{\frac{\mbox{OFFER}_{t}+\mbox{BID}_{t}}{2}}
}
(Venkataraman, 2001).
Note that the input of this function consists of the matched trades and
quotes, so this is where the time indication refers to (and thus not to the
registered quote timestamp).
}

\usage{
prop_price_impact(data);
}

\arguments{
  \item{data}{ 
xts object, containing joined trades and quotes (e.g. using \code{\link{matchtq()}})
}
}

\section{Details}{
}

\value{
an xts object containing the proportional price impact.
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