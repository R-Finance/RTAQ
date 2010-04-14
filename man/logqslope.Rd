\name{logqslope}
\Rdversion{1.1}
\alias{logqslope}
\title{
Log quoted slope
}

\description{
Function returns an xts object with the log quoted slope, defined as

\deqn{
\mbox{log quoted slope}_t =  \frac{\mbox{log quoted spread}_t}{\mbox{log quoted size}_t}.
}
}

\usage{
logqslope(data)
}

\arguments{
  \item{data}{ 
xts object, containing joined trades and quotes (e.g. using \code{\link{matchtq()}})
}
}

\section{Details}{
}

\value{
an xts object containing the log quoted slope.
}

\references{
}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd