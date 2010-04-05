\name{matchtq}
\Rdversion{1.1}
\alias{matchtq}
\title{
Match trade and quote data}

\description{
Function matches the trades and quotes and returns an xts-object containing
both tdata.}

\usage{
matchtq(tdata,qdata,adjustment=2)
}

\arguments{
\item{tdata}{ xts-object containing the trade data. Please see pdf documentation for the recommended format.}
\item{qdata}{ xts-object containing the quote data. Please see pdf documentation for the recommended format.}
\item{adjustment}{ numeric, number of seconds the quotes are registered faster than
the trades (should be round and positive). Based on the research of
Vergote (2005), we set 2 seconds as the default.}
}

\section{Details}{}

\value{xts-object containing the matched trade and quote data}

\references{
Vergote, O. (2005). How to match trades and quotes for NYSE stocks?
K.U.Leuven working paper.}

\author{ Jonathan Cornelissen and Kris Boudt}