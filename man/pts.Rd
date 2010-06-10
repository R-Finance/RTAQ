\name{pts}
\Rdversion{1.1}
\alias{pts}
\title{
Proportional half traded spread
}

\description{
Function returns an xts object with the proportional half traded spread, defined as

\deqn{
\mbox{proportional half traded spread}_t =  \frac{\mbox{half traded spread}_t}{\frac{\mbox{OFFER}_{t}+\mbox{BID}_{t}}{2}}.
}
Note that the input of this function consists of the matched trades and quotes,
 so this is were the time indication refers to (and thus not to the registered quote timestamp).
}

\usage{
pts(data);
}

\arguments{
  \item{data}{ 
xts object, containing joined trades and quotes (e.g. using \code{\link{matchtq}})
}
}

\section{Details}{
}

\value{
an xts object containing the proportional half traded spread.
}

\references{
}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd