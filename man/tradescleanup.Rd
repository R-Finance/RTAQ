\name{tradescleanup}
\Rdversion{1.1}
\alias{tradescleanup}
\title{
Perform a number of cleaning procedures on trade data
}
\description{
The function performs a bunch of cleaning procedures for trade
data for all stocks in "ticker" over the interval [from,to] and saves the
result in the folder datadestination. 

The following cleaning functions are performed sequentially:
\code{\link{nozeroprices}}, \code{\link{selectexchange}}, \code{\link{salescond}},
\code{\link{mergesametimestamp}}.

Since the function \code{\link{rmtradeoutliers}}
also requires cleaned quote data as input, it is not incorporated here and
there is a seperate wrapper called \code{\link{tradescleanup_finalop}}.
}

\usage{
tradescleanup(from,to,datasource,datadestination,ticker,exchange)
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
Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde, and N. Shephard (2010).
Realized kernels in practice: Trades and quotes. Econometrics Journal,
forthcoming.}

\author{ Jonathan Cornelissen and Kris Boudt}