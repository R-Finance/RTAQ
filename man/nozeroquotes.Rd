\name{nozeroquotes}
\Rdversion{1.1}
\alias{nozeroquotes}
\title{
Delete the observations where the bid or ask is zero
}
\description{
Function deletes the observations where the bid or ask is zero.
}

\usage{
nozeroquotes(qdata);
}

\arguments{
  \item{qdata}{an xts object at least containing the columns "BID" and "OFFER".} 
}

\section{Details}{}

\value{
xts object
}
\references{
}
\author{ Jonathan Cornelissen and Kris Boudt}