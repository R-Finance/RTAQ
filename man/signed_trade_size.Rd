\name{signed_trade_size}
\Rdversion{1.1}
\alias{signed_trade_size}
\title{
Signed trade size
}

\description{
Function returns an xts object with the signed trade size, defined as

\deqn{
\mbox{signed trade size}_t =  D_t * \mbox{SIZE}_{t},}

where \eqn{D_t} is 1 (-1) if \eqn{trade_t} was buy (sell).
}

\usage{
signed_trade_size(data)
}

\arguments{
  \item{data}{ 
xts object, containing joined trades and quotes (e.g. using \code{\link{matchtq()}})
}
}

\section{Details}{
}

\value{
an xts object containing the signed trade size.
}

\references{
}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd