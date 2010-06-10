\name{convert}
\Rdversion{1.1}
\alias{convert}
\title{
Convert trade or quote data into xts object saved in the RData format}

\description{
Function converts both trade and quote data stored as "txt" or "csv" and
structured as illustrated in the pdf documentation into xts-objects and 
stores them in the "RData" format. 
Subsequently, the data can be loaded into the R console by \code{\link{TAQload}}.
}

\usage{
convert(from,to,datasource,datadestination,trades=TRUE,quotes=TRUE,
ticker,dir=F,extention="txt",header=F,tradecolnames=NULL,
quotecolnames=NULL,format="\%m/\%d/\%Y \%H:\%M:\%S")
}

\arguments{
\item{from}{ first date to convert e.g. "2008-01-30".}
\item{to}{ last date to convert e.g. "2008-01-31".}
\item{datasource}{ folder in which the original data is stored.}
\item{datadestination}{ folder in which the converted data should be stored.}
\item{trades}{ boolean, determines whether trades are converted.}
\item{quotes}{ boolean, determines whether quotes are converted.}
\item{ticker}{ vector with tickers to be converted.}
\item{dir}{ boolean, if TRUE the datadestination folder and subfolders will be created automatically.}
\item{extention}{ character, indicating the data format of the original data. Can be either "txt" or "csv".}
\item{header}{ boolean, indicating whether each data file contains a header.}
\item{tradecolnames}{ vector containing column names of your trade data. By default, 
the standard NYSE data format is taken, see pdf documentation for more details.}
\item{quotecolnames}{ vector containing column names of your quote data. By default, 
the standard NYSE data format is taken, see pdf documentation for more details.}
\item{format}{ character, indicates in what format TIME and DATE are recorded in the original data.
By default, "\%m/\%d/\%Y \%H:\%M:\%S" is taken, which means the date is denoted by e.g. "01/30/2008" and the time by e.g. "09:30:00".}
}

\section{Details}{}

\value{For each day an xts object is saved into the folder of that date, containing the converted data.
This procedure is performed for each stock in "ticker". More information can be found in the pdf documentation.}

\references{}

\author{ Jonathan Cornelissen and Kris Boudt}