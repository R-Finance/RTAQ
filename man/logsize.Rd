\name{logsize}
\Rdversion{1.1}
\alias{logsize}
\title{
Log quoted size
}

\description{
Function returns an xts object with the log quoted size, defined as

\deqn{
\mbox{log quoted size}_t =  \log(\mbox{OFFERSIZE}_{t})-\log(\mbox{BIDSIZE}_{t})
}

(Hasbrouck and Seppi, 2001).
Note that the input of this function consists of the matched trades and
quotes, so this is where the time indication refers to (and thus not to the
registered quote timestamp).
}

\usage{
logsize(data)
}

\arguments{
  \item{data}{ 
xts object, containing joined trades and quotes (e.g. using \code{\link{matchtq()}})
}
}

\section{Details}{
}

\value{
an xts object containing the log quoted size.
}

\references{
Hasbrouck, J. and D. J. Seppi (2001). 
Common factors in prices, order flows and liquidity. 
Journal of Financial Economics, 383-411.
}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd