\name{price_impact}
\Rdversion{1.1}
\alias{price_impact}
\title{
Price impact
}

\description{
Function returns an xts object with the price impact, defined as

\deqn{
\mbox{price impact}_t =  \frac{\mbox{effective spread}_t - \mbox{realized spread}_t}{2}
}
(see Bhoehmer (2005), Bessembinder (2003)). 
}

\usage{
price_impact(data);
}

\arguments{
  \item{data}{ 
xts object, containing joined trades and quotes (e.g. using \code{\link{matchtq}})
}
}

\section{Details}{
}

\value{
an xts object containing the price impact.
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