\name{agg_quotes}
\Rdversion{1.1}
\alias{agg_quotes}
\title{
Aggregate an xts object containing quote data}


\description{
Function returns an xts object containing the aggregated quote data with columns "SYMBOL", "EX", "BID","BIDSIZE","OFFER","OFFERSIZE". 
Please see pdf documentation for the requirements regarding "quote data objects".
}

\usage{
agg_quotes(qdata,on="minutes",k=5)
}

\arguments{
\item{qdata}{ xts object, containing the quote data. See pdf documentation for more details.}
\item{on}{ character, indicating the time scale in which "k" is expressed. Possible values are: “secs”, “seconds”, “mins”, “minutes”,“hours”.}
\item{k}{ positive integer, indicating the number of periods to aggregate over. E.g. to aggregate a 
xts object to the 5 minute frequency, set k=5 and on="minutes".}
}

\section{Details}{
The timestamps of the new time series are the closing times of the intervals. 

Please Note:

In case an interval is empty, it is omitted. (Possibly, you expect an NA but this isn't the case)

Column "MODE" is dropped because aggregating makes no sense.

It is assumed that a trading day starts at 09:30:00 and ends at 16:00:00. Returned object will always contain the 
first observation (opening price,..).

It is recommended to use an object containing one day of data as input.
}

\value{
An xts object containing the aggregated quote data.
}

\references{}

\author{ Jonathan Cornelissen and Kris Boudt}