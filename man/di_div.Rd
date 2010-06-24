\name{di_div}
\Rdversion{1.1}
\alias{di_div}
\title{
Depth imbalance (as a ratio)
}

\description{
Function returns an xts object with the depth imbalance (as a ratio), defined as

\deqn{
\mbox{depth imbalace (as ratio)}_t =  (\frac{D_t *\mbox{OFRSIZ}_{t}}{\mbox{BIDSIZ}_{t}})^{D_t},
}
where \eqn{D_t} is 1 (-1) if \eqn{trade_t} was buy (sell) (see Bhoehmer (2005), Bessembinder (2003)). 
Note that the input of this function consists of the matched trades and quotes,
 so this is were the time indication refers to (and thus not to the registered quote timestamp).
}

\usage{
di_div(data);
}

\arguments{
  \item{data}{ 
xts object, containing joined trades and quotes (e.g. using \code{\link{matchtq}})
}
}

\section{Details}{
}

\value{
an xts object containing the depth imbalace (as a ratio).
}

\references{
Bessembinder, H. (2003). Issues in assessing trade execution costs. 
Journal of Financial Markets, 223-257.

Boehmer, E. (2005). Dimensions of execution quality: Recent evidence for US
equity markets. Journal of Financial Economics 78 (3), 553-582.
}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd