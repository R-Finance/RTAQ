\name{es}
\Rdversion{1.1}
\alias{es}
\title{
Effective spread
}

\description{
Function returns an xts object with the effective spread, defined as

\deqn{
\mbox{Effective Spread}_t =  2*D_t*(\mbox{PRICE}_{t} - \frac{(\mbox{BID}_{t}+\mbox{OFFER}_{t})}{2}),
}

where \eqn{D_t} is 1 (-1) if \eqn{trade_t} was buy (sell) (see Bhoehmer (2005), Bessembinder (2003)). 
Note that the input of this function consists of the matched trades and quotes,
 so this is were the time indication refers to (and thus not to the registered quote timestamp).
}

\usage{
es(data);
}

\arguments{
  \item{data}{ 
xts object, containing joined trades and quotes (e.g. using \code{\link{matchtq()}})
}
}

\section{Details}{
}

\value{
an xts object containing the effective spread.
}

\references{
Bessembinder, H. (2003). Issues in assessing trade execution costs. Journal of
Financial Markets, 223-257.

Boehmer, E. (2005). Dimensions of execution quality: Recent evidence for us
equity markets. Journal of Financial Economics 78 (3), 553-582.
}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd