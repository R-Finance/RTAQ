\name{tradescleanup_finalop}
\Rdversion{1.1}
\alias{tradescleanup_finalop}
\title{
Perform a final cleaning procedure on trade data}

\description{
Function performs cleaning procedure \code{\link{rmtradeoutliers}} 
for the trades of all stocks in "ticker" over the interval 
[from,to] and saves the result in "datadestination". 
Note that preferably the input data for this function 
is trade and quote data cleaned by respectively e.g. \code{\link{tradescleanup}}
and \code{\link{quotescleanup}}.
}

\usage{
tradescleanup_finalop(from="2008-01-03",to="2008-01-03",datasource,datadestination,ticker)
}

\arguments{
\item{from}{character indicating first date to clean, e.g. "2008-01-30".}
\item{to}{character indicating last date to clean, e.g. "2008-01-31".}
\item{datasource}{character indicating the folder in which the original data is stored.}
\item{datadestination}{character indicating the folder in which the cleaned data is stored.}
\item{ticker}{vector of tickers for which the data should be cleaned.}
}

\section{Details}{}

\value{For each day an xts object is saved into the folder of that date, containing the cleaned data.
This procedure is performed for each stock in "ticker".}

\references{
Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde, and N. Shephard (2010).
Realized kernels in practice: Trades and quotes. Econometrics Journal,
forthcoming.}

\author{ Jonathan Cornelissen and Kris Boudt}