\name{qs}
\Rdversion{1.1}
\alias{qs}
\title{
Quoted spread
}

\description{
Function returns an xts object with the quoted spread, defined as

\deqn{
\mbox{quoted spread}_t =  \mbox{OFR}_{t}-\mbox{BID}_{t}
}
Note that the input of this function consists of the matched trades and
quotes, so this is where the time indication refers to (and thus not to the
registered quote timestamp).
}

\usage{
qs(data)
}

\arguments{
  \item{data}{ 
xts object, containing joined trades and quotes (e.g. using \code{\link{matchtq}})
}
}

\section{Details}{
}

\value{
an xts object containing the quoted spread.
}

\references{
}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd