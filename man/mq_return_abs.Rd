\name{mq_return_abs}
\Rdversion{1.1}
\alias{mq_return_abs}
\title{
Midquote absolute return
}

\description{
Function returns an xts object with the midquote absolute returns, defined as

\deqn{
\mbox{midquote absolute return}_t =  |\log(\mbox{midquote}_{t})-\log(\mbox{midquote}_{t-1})|,
}

where \eqn{\mbox{midquote}_{t} = \frac{\mbox{BID}_{t} + \mbox{OFR}_{t}}{2}}.
}

\usage{
mq_return_abs(data)
}

\arguments{
  \item{data}{ 
xts object, containing joined trades and quotes (e.g. using \code{\link{matchtq}})
}
}

\section{Details}{
}

\value{
an xts object containing midquote absolute returns.
}

\references{
}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd