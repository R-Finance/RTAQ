\name{salescond}
\Rdversion{1.1}
\alias{salescond}
\title{
Delete entries with abnormal Sale Condition.
}
\description{
Function deletes entries with abnormal Sale Condition: 
trades where column "COND" has
a letter code, except for "E" and "F".
}

\usage{
salescond(tdata)
}

\arguments{
  \item{tdata}{ an xts object containing the time series data, with 
one column named "COND" indicating the Sale Condition. }
}

\section{Details}{}

\value{
xts object
}
\references{
}
\author{ Jonathan Cornelissen and Kris Boudt}