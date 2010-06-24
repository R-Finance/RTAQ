\name{rs}
\Rdversion{1.1}
\alias{rs}
\title{
Realized spread
}

\description{
Function returns an xts object with the realized spread, defined as

\deqn{
\mbox{Realized Spread}_t =  2*D_t*(\mbox{PRICE}_{t} - \frac{(\mbox{BID}_{t+300}+\mbox{OFR}_{t+300})}{2}),
}

where \eqn{D_t} is 1 (-1) if \eqn{trade_t} was buy (sell) (see Bhoehmer (2005), Bessembinder (2003)). 
Note that the time indication of \eqn{\mbox{BID}} and \eqn{\mbox{OFR}} refers 
to the registered time of the quote in seconds.
}

\usage{
rs(data,tdata,qdata);
}

\arguments{
  \item{data}{ 
xts object, containing joined trades and quotes (e.g. using \code{\link{matchtq()}})
}
  \item{tdata}{xts object, containing the trade data. See pdf documentation for more details.}
  \item{qdata}{xts object, containing the quote data. See pdf documentation for more details.}
}

\section{Details}{
}

\value{
an xts object containing the realized spread.
}

\references{
Bessembinder, H. (2003). Issues in assessing trade execution costs. Journal of
Financial Markets, 223-257.

Boehmer, E. (2005). Dimensions of execution quality: Recent evidence for US
equity markets. Journal of Financial Economics 78 (3), 553-582.
}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd