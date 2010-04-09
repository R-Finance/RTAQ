\name{RBPVar}
\Rdversion{1.1}
\alias{RBPVar}
\title{
Realized BiPower Variance
}
\description{
Function returns the Realized BiPower Variance (RBPVar), defined in Barndorff-Nielsen and Shephard (2004).

Let \eqn{r_{t,i}} be a return (with \eqn{i=1,\ldots,M}) in period \eqn{t}.

Then, the RBPVar is given by
\deqn{
\mbox{RBPVar}_{t}= \frac{\pi}{2} \sum_{i=2}^{M} |r_{t,i}| |r_{t,i-1}|
}

}

\usage{
RBPVar(data)
}

\arguments{
  \item{data}{ a vector/zoo/xts object containing all returns in period t for one asset. }
}

\section{Details}{
}

\value{
numeric
}

\references{
Barndorff-Nielsen, O. and N. Shephard (2004). Power and bipower variation with 
stochastic volatility and jumps. Journal of Financial Econometrics 2 (1), 1-37.}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd