\name{mergequotessametimestamp}
\Rdversion{1.1}
\alias{mergequotessametimestamp}
\title{
Merge multiple quote entries with the same time stamp
}
\description{
Function replaces multiple quote entries that have the same time stamp 
by a single one and returns an xts object with unique time stamps only.
}

\usage{
mergequotessametimestamp(qdata,selection="median")
}

\arguments{
  \item{tdata}{ an xts object containing the time series data, with 
at least two columns named "BID" and "OFFER" indicating the bid and ask price 
and two columns "BIDSIZE", "OFFERSIZE" indicating the number of round lots available at these 
prices. }
  \item{selection}{indicates how the bid and ask price for a certain time stamp
should be calculated in case of multiple observation for a certain time
stamp. By default, selection="median", and the median price is taken. Alternatively:
\itemize{
\item selection = "maxvolume": use the (bid/ask) price of the entry with
largest (bid/ask) volume.
\item selection = "weightedaverage": take the weighted average of all bid (ask) prices,
weighted by "BIDSIZE" ("OFFERSIZE").
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