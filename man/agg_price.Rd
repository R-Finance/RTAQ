\name{agg_price}
\Rdversion{1.1}
\alias{agg_price}
\title{
Aggregate a time series but keep first and last observation}

\description{
Function returns new time series as xts object where first observation is always the opening price
and subsequent observations are the closing prices over the interval with as endpoint the timestamp 
of the result.
}

\usage{
agg_price = function(ts,FUN = previoustick,on="minutes",k=5)
}

\arguments{
\item{ts}{ xts object, containing the original time series.}
\item{FUN}{ function to apply over each interval. By default, previous tick aggregation is done.}
\item{on}{ character, indicating the time scale in which "k" is expressed. Possible values are: "secs", "seconds", "mins", "minutes","hours".}
\item{k}{ positive integer, indicating the number of periods to aggregate over. E.g. to aggregate a 
xts object to the 5 minute frequency set k=5 and on="minutes".}
}

\section{Details}{
The timestamps of the new time series are the closing times and/or days of the intervals. 

In case of previous tick aggregation, 
for on="seconds"/"minutes"/"hours",
 the element of the returned series with e.g. timestamp 09:35:00 contains 
the last observation up to that point, excluding the value at 09:35:00 itself. An exception 
is 16:00:00, where the observation at 16:00:00 is included in the interval, since this is the
end of a trading day at the NYSE.

Please Note:

In case an interval is empty, it is omitted. (Possibly, you expect an NA but this isn't the case)

It is assumed that a trading day starts at 09:30:00 and ends at 16:00:00.

It is recommended to use a time series containing one day as input.
}

\value{
An xts object containing the aggregated time series.
}

\references{}

\author{ Jonathan Cornelissen and Kris Boudt}