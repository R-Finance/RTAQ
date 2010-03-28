\name{rmtradeoutliers}
\Rdversion{1.1}
\alias{rmtradeoutliers}
\title{
Delete transactions with unlikely transaction prices
}
\description{
Function deletes entries with prices that are above the ask plus the bid-ask spread.
Similar for entries with prices below the bid minus the bid-ask spread.}

\usage{
rmtradeoutliers(tdata,qdata)
}

\arguments{
  \item{tdata}{ an xts object containing the time series data, with 
at least the column "PRICE" indicating the transaction price.}
  \item{qdata}{ an xts object containing the time series data,
with at least the columns "BID" and "OFFER" indicating the bid and 
ask prices.
}
}

\section{Details}{
Note: in order to work correctly, the input data of this function should be
cleaned trade (tdata) and quote (qdata) data respectively.
}

\value{
xts object
}
\references{
}
\author{ Jonathan Cornelissen and Kris Boudt}