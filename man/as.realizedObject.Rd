\name{as.realizedObject}
\Rdversion{1.1}
\alias{as.realizedObject}
\title{Convert xts object to a realized object}

\description{Function converts an xts timeseries (one dimensional, possibly multiple days) into 
a realized object from the package "realized" by Scott Payseur.}

\usage{as.realizedObject( ts, cts = FALSE , makeReturns = TRUE )}

\arguments{
\item{ts}{ xts object, containing the original time series (one dimensional, possibly multiple days).}
\item{cts}{ Calendar time sampling or tick time sampling. See realizedObject function in "realized" package by Scott Payseur.}
\item{makeReturns}{ boolean, indicates whether you pass prices. TRUE by default. See realizedObject function in "realized" package by Scott Payseur.}
}

\value{A realized object.}

\author{ Jonathan Cornelissen and Kris Boudt. Initial code by Eric Zivot}
\keyword{data manipulation}

\examples{
#one day of data: 
data("sample_tdata");
x = as.realizedObject(sample_tdata$PRICE);

#multiple days:
data("sample_5minprices");
ts1 = sample_5minprices$stock1;
#construct realized object:
x = as.realizedObject(ts1);
#x now contains a list with a realized object for each day
#see manual of the "realized" package by Scott Payseur for more info.
}