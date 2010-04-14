\name{liquidity}
\Rdversion{1.1}
\alias{liquidity}
\title{
Calculate numerous (22) liquidity measures
}

\description{
Function returns an xts object containing the following liquidity measures:

\code{\link{es}}, \code{\link{rs}}, \code{\link{value_trade}}, \code{\link{signed_value_trade}},  
       \code{\link{di_diff}}, \code{\link{di_div}}, \code{\link{pes}}, \code{\link{prs}}, \code{\link{price_impact}}, \code{\link{prop_price_impact}},  
       \code{\link{tspread}}, \code{\link{pts}}, \code{\link{p_return_sqr}}, \code{\link{p_return_abs}}, \code{\link{qs}}, \code{\link{pqs}},  
       \code{\link{logqs}}, \code{\link{logsize}}, \code{\link{qslope}}, \code{\link{logqslope}}, \code{\link{mq_return_sqr}},  
       \code{\link{mq_return_abs}}.
}

\usage{
liquidity(data,tdata,qdata)
}

\arguments{
  \item{data}{ 
xts object, containing joined trades and quotes (e.g. using \code{\link{matchtq()}})
}
\item{tdata}{ xts-object containing the trade data. Please see pdf documentation for the recommended format.}
\item{qdata}{ xts-object containing the quote data. Please see pdf documentation for the recommended format.}
}

\section{Details}{
}

\value{
an xts object the above mentioned liquidity measures.
}

\references{
}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd