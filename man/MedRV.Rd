\name{MedRV}
\Rdversion{1.1}
\alias{MedRV}
\title{
MedRV
}
\description{
Function returns the MedRV, defined in Andersen et al. (2009).

Let \eqn{r_{t,i}} be a return (with \eqn{i=1,\ldots,M}) in period \eqn{t}.

Then, the MedRV is given by
\deqn{
\mbox{MedRV}_{t}=\frac{\pi}{6-4\sqrt{3}+\pi}\left(\frac{M}{M-2}\right) \sum_{i=2}^{M-1} \mbox{med}(|r_{t,i-1}|,|r_{t,i}|, |r_{t,i+1}|)^2
}

}

\usage{
MedRV(data)
}

\arguments{
  \item{data}{ a zoo/xts object containing all returns in period t for one asset. }
}

\section{Details}{
}

\value{
numeric
}

\references{
Andersen, T. G., D. Dobrev, and E. Schaumburg (2009). Jump-robust volatility 
estimation using nearest neighbor truncation. NBER Working Paper No.
15533.}

\author{ Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd