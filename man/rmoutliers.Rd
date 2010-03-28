\name{rmoutliers}
\Rdversion{1.1}
\alias{rmoutliers}
\title{
Delete entries for which the mid-quote is outlying with to respect surrounding entries
}
\description{
If type="standard":  Function deletes entries for which the mid-quote deviated by more than "maxi"
median absolute deviations from a rolling centered median (excluding
the observation under consideration) of "window" observations.

If type="advanced":  Function deletes entries for which the mid-quote deviates by more than "maxi"
median absolute deviations from the value closest to the midquote of
these three options:
\enumerate{
\item Rolling centered median (excluding the observation under consid-
eration)
\item Rolling median of the following "window" observations
\item Rolling median of the previous "window" observations
}

The advantage of this procedure compared to the "standard" proposed
by Barndorff-Nielsen et al. (2009) is that it will not incorrectly remove
large price jumps. Therefore this procedure has been set as the default
for removing outliers.
}

\usage{
rmoutliers(qdata,maxi=10,window=50,type="advanced")}

\arguments{
  \item{qdata}{an xts object at least containing the columns "BID" and "OFFER".} 
  \item{maxi}{an integer, indicating the maximum number of median absolute deviations allowed.} 
  \item{window}{an integer, indicating the time window for which the "outlyingness" is considered.}
  \item{type}{should be "standard" or "advanced" (see description).}
}

\section{Details}{NOTE: This function works only correct if supplied input data consists of 1 day!!}

\value{xts object}

\references{
Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde, and N. Shephard (2009). 
Multivariate realised kernels: consistent positive semi-denite
estimators of the covariation of equity prices with noise and non-synchronous trading.
working paper.
}

\author{ Jonathan Cornelissen and Kris Boudt}