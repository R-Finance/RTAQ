\name{di_diff}
\Rdversion{1.1}
\alias{di_diff}
\title{
Depth imbalance (as a difference)
}

\description{
Function returns an xts object with the depth imbalance (as a difference), defined as

\deqn{
\mbox{depth imbalace (as difference)}_t =  \frac{D_t *(\mbox{OFFERSIZE}_{t}-\mbox{BIDSIZE}_{t})}{(\mbox{OFFERSIZE}_{t}+\mbox{BIDSIZE}_{t})},
}
where \eqn{D_t} is 1 (-1) if \eqn{trade_t} was buy (sell) (see Bhoehmer (2005), Bessembinder (2003)). 
Note that the input of this function consists of the matched trades and quotes,
 so this is were the time indication refers to (and thus not to the registered quote timestamp).
}

\usage{
di_diff(data);
}

\arguments{
  \item{data}{ 
xts object, containing joined trades and quotes (e.g. using \code{\link{matchtq}})
}
}

\section{Details}{
}

\value{
an xts object containing the depth imbalace (as difference).
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