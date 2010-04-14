\name{qslope}
\Rdversion{1.1}
\alias{qslope}
\title{
Quoted slope
}

\description{
Function returns an xts object with the quoted slope, defined as

\deqn{
\mbox{quoted slope}_t =  \frac{\mbox{quoted spread}_t}{\mbox{log quoted size}_t}
}
(Hasbrouck and Seppi, 2001).
}

\usage{
qslope(data)
}

\arguments{
  \item{data}{ 
xts object, containing joined trades and quotes (e.g. using \code{\link{matchtq()}})
}
}

\section{Details}{
}

\value{
an xts object containing the quoted slope.
}

\references{
Hasbrouck, J. and D. J. Seppi (2001). 
Common factors in prices, order flows and liquidity. 
Journal of Financial Economics, 383-411.
}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd