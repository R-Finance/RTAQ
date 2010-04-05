\name{TAQload}
\Rdversion{1.1}
\alias{TAQload}
\title{
Load trade or quote data into R}

\description{
Function to load the taq data into R. If only the trades (or
quotes) should be loaded the function returns them directly as xts object.
If both trades and quotes should be extracted, the function returns a list
with two items named "trades" and "quotes" respectively.

The function assumes that the files are ordered in folders per day, and the folders
contain: ticker_trades.RData or ticker_quotes.RData. In these files
the xts object is stored. If you used the function \code{\link{convert}}, 
this shall be the case. Please find more information in the pdf documentation.
}

\usage{
TAQload(tickers,from,to,trades=TRUE,quotes=FALSE,datasource=NULL,variables=NULL)
}

\arguments{
\item{tickers}{ the ticker(s) to be loaded.
It is recommended that you use only 1 ticker as input in case of non-synchronic observations. For synchronic data a vector of tickers can be used.}
\item{from}{ first day to load e.g. "2008-01-30"}
\item{to}{ last day to load e.g. "2008-01-30"}
\item{trades}{ boolean, determines whether trades are extracted}
\item{quotes}{ boolean, determines whether quotes are extracted}
\item{datasource}{ path to folder in which the files are contained}
\item{variables}{ a character (or character vector) containing the name(s) of the variable(s) that should be loaded, e.g. c("SYMBOL","PRICE"). By default all data is loaded.}
}

\section{Details}{}

\value{see section \code{description}}

\references{}

\author{ Jonathan Cornelissen and Kris Boudt}