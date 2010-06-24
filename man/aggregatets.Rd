\name{aggregatets}
\Rdversion{1.1}
\alias{aggregatets}
\title{
Aggregate a time series}

\description{
Function returns aggregated time series as xts object. 
It can handle irregularly spaced timeseries and returns a regularly spaced one.
}

\usage{
aggregatets(ts, FUN=previoustick, on="minutes", k=1, weights=NULL,dropna=F)
}

\arguments{
\item{ts}{ xts object, containing the original time series.}
\item{FUN}{ function to apply over each interval. By default, previous tick aggregation is done. 
In case weights are supplied, this argument is ignored and a weighted average is taken.}
\item{on}{ character, indicating the time scale in which "k" is expressed. Possible values are: "secs", "seconds", "mins", "minutes","hours", "days", "weeks".}
\item{k}{ positive integer, indicating the number of periods to aggregate over. E.g. to aggregate a 
xts object to the 5 minute frequency set k=5 and on="minutes".}
\item{weights}{ 
By default, no weighting scheme is used. 
When you assign an xts object with wheights to this argument, a weighted mean is taken over each interval. 
Of course, the weights should have the same timestamps as the supplied time series.
}
\item{dropna}{ boolean, which determines whether empty intervals should be dropped.
By default, an NA is returned in case an interval is empty.}
}

\section{Details}{
The timestamps of the new time series are the closing times and/or days of the intervals. 
E.g. for a weekly aggregation the new timestamp is the last day in that particular week (namely sunday).

In case of previous tick aggregation, 
for on="seconds"/"minutes"/"hours",
 the element of the returned series with e.g. timestamp 09:35:00 contains 
the last observation up to that point, excluding the value at 09:35:00 itself.

Please Note:
In case an interval is empty, by default an NA is returned. In case e.g. previous 
tick aggregation it makes sense to fill these NA's by the function \code{na.locf}
(last observation carried forward) from the zoo package.
}

\value{
An xts object containing the aggregated time series.
}

\references{}

\author{ Jonathan Cornelissen and Kris Boudt}