\name{signed_value_trade}
\Rdversion{1.1}
\alias{signed_value_trade}
\title{
Signed trade value
}

\description{
Function returns an xts object with the signed trade value, defined as

\deqn{
\mbox{signed trade value}_t =  D_t * (\mbox{SIZE}_{t}*\mbox{PRICE}_{t}),}

where \eqn{D_t} is 1 (-1) if \eqn{trade_t} was buy (sell) 
(see Bhoehmer (2005), Bessembinder (2003)).

}
\usage{
signed_value_trade(data)
}

\arguments{
  \item{data}{ 
xts object, containing joined trades and quotes (e.g. using \code{\link{matchtq}})
}
}

\section{Details}{
}

\value{
an xts object containing the signed trade value.
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