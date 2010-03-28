\name{quotescleanup}
\Rdversion{1.1}
\alias{quotescleanup}
\title{
Perform a bunch of cleaning procedures on quote data
}

\description{
The function performs a bunch of cleaning procedures for quote 
data for all stocks in "ticker" over the interval [from,to] and saves the
result in the folder datadestination. 

The following cleaning functions are performed sequentially:
\code{\link{nozeroquotes}}, \code{\link{selectexchange}}, \code{\link{rmlargespread}},
\code{\link{mergequotessametimestamp}}, \code{\link{rmoutliers}}.
}

\usage{
quotescleanup(from,to,datasource,datadestination,ticker,exchange)
}

\arguments{
\item{from}{character indicating first date to clean, e.g. "2008-01-30".}
\item{to}{character indicating last date to clean, e.g. "2008-01-31".}
\item{datasource}{character indicating the folder in which the original data is stored.}
\item{datadestination}{character indicating the folder in which the cleaned data is stored.}
\item{ticker}{vector of tickers for which the data should be cleaned.}
\item{exchange}{vector of stock exchange symbols for all tickers in vector "ticker".
The possible exchange symbols are:
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

\value{For each day an xts object is saved into the folder of that date, containing the cleaned data.
This procedure is performed for each stock in "ticker".}

\references{
Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde, and N. Shephard (2009). 
Multivariate realised kernels: consistent positive semi-denite
estimators of the covariation of equity prices with noise and non-synchronous trading.
working paper.
}

\author{ Jonathan Cornelissen and Kris Boudt}