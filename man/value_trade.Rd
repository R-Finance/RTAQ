\name{value_trade}
\Rdversion{1.1}
\alias{value_trade}
\title{
Trade value
}

\description{
Function returns an xts object with the trade value, defined as

\deqn{
\mbox{trade value}_t =  \mbox{SIZE}_{t}*\mbox{PRICE}_{t}.
}
}

\usage{
value_trade(data);
}

\arguments{
  \item{data}{ 
xts object, containing the trade data, with at least a column "SIZE", indicating
the transaction volume and a column "PRICE", indicating the transaction price.
}
}

\section{Details}{
}

\value{
an xts object containing the trade value.
}

\references{
}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd