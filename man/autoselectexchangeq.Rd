\name{autoselectexchangeq}
\Rdversion{1.1}
\alias{autoselectexchangeq}
\title{
Retain only data from the stock exchange with the highest volume
}
\description{
Function returns an xts object containing only observations 
of the exchange with highest
value for the sum of "BIDSIZ" and "OFRSIZ", i.e. the highest quote volume.
}

\usage{
autoselectexchangeq(qdata)
}

\arguments{
  \item{qdata}{ 
an xts object with at least a column "EX", indicating the exchange symbol 
and columns "BIDSIZ" and "OFRSIZ", indicating 
the volume available at the bid and ask respectively.
The chosen exchange is printed on the console.
The possible exchanges are:
\itemize{
\item A: AMEX
\item N: NYSE
\item B: Boston
\item P: Arca
\item C: NSX
\item T/Q: NASDAQ
\item D: NASD ADF and TRF
\item X: Philadelphia
\item I: ISE
\item M: Chicago
\item W: CBOE
\item Z: BATS
}
}
}
\section{Details}{}

\value{
xts object
}
\references{
}
\author{ Jonathan Cornelissen and Kris Boudt}